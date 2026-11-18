(ns specel.core
  (:refer-clojure :exclude [def])
  (:require [clojure.spec.alpha :as s]
            [hegel-clj.core :as h]
            [hegel-clj.generator :as g])
  (:import (dev.hegel AssumeRejected Generator Label)
           (java.net URI)
           (java.util Date UUID)))

(def ^:dynamic *recursion-limit*
  "A soft limit on how many times a branching spec (or/alt/*/opt-keys/multi-spec)
  can be recursed through during generation. After this a
  non-recursive branch will be chosen."
  4)

(defn- enter-branch
  [registry id]
  (update-in registry [::recursion id] (fnil inc 0)))

(defn- recursion-limit?
  [registry id path k]
  (and (> (get-in registry [::recursion id] 0) (::recursion-limit registry))
       (contains? (set path) k)))

(defn- no-finite-generator
  [spec path]
  (throw (ex-info "No finite generator within recursion limit"
                  {::no-finite-generator true :spec spec :path path})))

(defn- productive-generator
  [f]
  (try
    (f)
    (catch clojure.lang.ExceptionInfo e
      (when-not (::no-finite-generator (ex-data e))
        (throw e)))))

(defn- enter-spec
  [spec registry overrides path ancestors]
  (let [n (get ancestors spec 0)
        limit (max 1 (::recursion-limit registry))
        override-depth (reduce (fn [depth p]
                                 (if (and (vector? p)
                                          (< (count path) (count p))
                                          (= path (subvec p 0 (count path))))
                                   (max depth (count p))
                                   depth))
                               0 (keys overrides))]
    (when (>= n (+ limit override-depth))
      (no-finite-generator spec path))
    (let [counts (java.util.IdentityHashMap. ^java.util.Map ancestors)]
      (.put counts spec (inc n))
      counts)))

(defn- valid-candidate?
  [spec value]
  (s/valid? spec (if-let [retag (::retag (meta spec))]
                   (retag value)
                   value)))

(defn- filter-generator
  [predicate ^Generator source]
  (reify Generator
    (doDraw [_ test-case]
      (loop [attempt 0]
        (when (= 100 attempt)
          (throw (AssumeRejected.)))
        (.startSpan test-case Label/FILTER)
        (let [discard? (volatile! true)
              [accepted? value]
              (try
                (let [value (.doDraw source test-case)
                      accepted? (boolean (predicate value))]
                  (vreset! discard? (not accepted?))
                  [accepted? value])
                (finally
                  (.stopSpan test-case @discard?)))]
          (if accepted?
            value
            (recur (inc attempt))))))))

(defn- captured
  [spec field-name]
  (try
    (let [field (.getDeclaredField (class spec) field-name)]
      (.setAccessible field true)
      (.get field spec))
    (catch NoSuchFieldException _ nil)))

(def ^:private predicate-spec-class (class (s/spec any?)))

(defn- finite-domain-size
  ([spec]
   (finite-domain-size spec #{}))
  ([spec seen]
   (when-not (contains? seen spec)
     (cond
       (set? spec) (count spec)
       (qualified-ident? spec) (if-let [registered (s/get-spec spec)]
                                 (finite-domain-size registered (conj seen spec))
                                 (cond
                                   (= `boolean? spec) 2
                                   (contains? #{`nil? `true? `false?} spec) 1))
       (and (= predicate-spec-class (class spec))
            (not (captured spec "cpred_QMARK_")))
       (finite-domain-size (captured spec "pred") (conj seen spec))
       (= boolean? spec) 2
       (contains? #{nil? true? false?} spec) 1))))

(defn- distinct-size-options
  [opts spec]
  (if-let [capacity (finite-domain-size spec)]
    (do
      (when (> (get opts :min-size 0) capacity)
        (throw (ex-info "Distinct collection size exceeds finite domain"
                        {:spec spec :min-count (:min-size opts) :capacity capacity})))
      (update opts :max-size min capacity))
    opts))

(defn- collection-generator
  [[_ element & {:keys [kind count min-count max-count distinct into gen-max]
                 :or {gen-max 20}}] gen]
  (let [kind (if (symbol? kind) (some-> (resolve kind) deref) kind)
        family (if (some? into)
                 (empty into)
                 (if kind (first (filter kind [[] () #{} {}])) []))
        opts (cond-> {:max-size (or max-count (max gen-max (* 2 (or min-count 0))))}
               min-count (assoc :min-size min-count)
               max-count (assoc :max-size max-count)
               count (assoc :min-size count :max-size count))
        opts (if (or distinct (set? family))
               (distinct-size-options opts element)
               opts)
        elements (when-not (= 0 (:max-size opts))
                   (if (zero? (get opts :min-size 0))
                     (productive-generator #(gen element))
                     (gen element)))
        vectors (if elements
                  (if (or distinct (set? family))
                    (g/fmap vec (g/set opts elements))
                    (g/vector opts elements))
                  (g/just []))
        entry? #(or (map-entry? %) (and (vector? %) (= 2 (clojure.core/count %))))
        result (cond
                 (vector? family) vectors
                 (list? family) (g/fmap #(apply list %) vectors)
                 (set? family) (g/fmap #(clojure.core/into family %) vectors)
                 (map? family) (g/fmap #(when (every? entry? %)
                                          (clojure.core/into family %))
                                       vectors)
                 :else (throw (ex-info "Unsupported collection kind" {:kind kind})))]
    (filter-generator (or kind coll?) result)))

(defn- keys-generator
  [[_ & {:keys [req req-un opt opt-un]}] gen]
  (let [missing (Object.)
        unqualify #(keyword (name %))
        entries (into {} (concat (map #(vector % %) (filter keyword? (flatten req)))
                                 (map #(vector (unqualify %) %)
                                      (filter keyword? (flatten req-un)))
                                 (map #(vector % %) opt)
                                 (map #(vector (unqualify %) %) opt-un)))
        optional (set (concat opt (map unqualify opt-un)))]
    (when-not (every? qualified-keyword? (vals entries))
      (throw (ex-info "Expected qualified spec names in keys"
                      {:req req :req-un req-un})))
    (let [generators (into {} (map (fn [[k spec]]
                                     [k (productive-generator #(gen spec k))]))
                           entries)]
      (letfn [(required-keys [requirement key-fn]
                (if (keyword? requirement)
                  (if (get generators (key-fn requirement))
                    (g/just #{(key-fn requirement)})
                    (no-finite-generator requirement [(key-fn requirement)]))
                  (case (name (first requirement))
                    "and" (g/fmap #(into #{} cat %)
                                  (g/tuple* (mapv #(required-keys % key-fn) (rest requirement))))
                    "or" (let [branches (into [] (keep #(productive-generator
                                                         (fn [] (required-keys % key-fn))))
                                              (rest requirement))]
                           (if (seq branches)
                             (g/one-of branches)
                             (no-finite-generator requirement []))))))]
        (let [required (g/fmap #(into #{} cat %)
                               (g/tuple* (vec (concat (map #(required-keys % identity) req)
                                                      (map #(required-keys % unqualify) req-un)))))]
          (g/bind
           (fn [required]
             (g/fmap (fn [m]
                       (into {} (remove (fn [[_ v]] (identical? missing v))) m))
                     (g/hmap (into {}
                                   (keep (fn [[k generator]]
                                           (when generator
                                             (cond
                                               (contains? required k) [k generator]
                                               (contains? optional k)
                                               [k (g/one-of [(g/just missing) generator])]))))
                                   generators))))
           required))))))

(defn- map-generator
  [[op k v & {:keys [count min-count max-count kind into gen-max]
              :or {gen-max 20}}] gen]
  (if (and (= op `s/every-kv) kind (not= kind `map?))
    (collection-generator (list `s/coll-of (s/tuple k v)
                                :kind kind :count count :min-count min-count
                                :max-count max-count :into into :gen-max gen-max)
                          (fn [_] (g/tuple (gen k 0) (gen v 1))))
    (let [empty-generator #(g/just (if into (empty into) {}))
          build (fn []
                  (let [keys (gen k 0)
                        values (gen v 1)
                        opts (distinct-size-options
                              {:min-size (or count min-count 0)
                               :max-size (or count max-count (max gen-max (* 2 (or min-count 0))))
                               :unique? true}
                              k)
                        generator (g/bind (fn [ks]
                                            (g/fmap #(zipmap ks %)
                                                    (g/vector {:size (clojure.core/count ks)} values)))
                                          (g/vector opts keys))]
                    (if into
                      (g/fmap #(clojure.core/into (empty into) %) generator)
                      generator)))]
      (cond
        (= 0 (or count max-count)) (empty-generator)
        (zero? (or count min-count 0)) (or (productive-generator build) (empty-generator))
        :else (build)))))

(defn- double-generator
  [[_ & opts] _]
  (let [opts (apply hash-map opts)]
    (g/double (cond-> (dissoc opts :NaN? :infinite?)
                (contains? opts :NaN?) (assoc :nan? (:NaN? opts))
                (contains? opts :infinite?) (assoc :infinity? (:infinite? opts))))))

(defn- integer-generator
  [[_ start end] _]
  (when-not (< start end)
    (throw (ex-info "Empty integer range" {:start start :end end})))
  (g/long {:min start :max (dec end)}))

(defn- and-generator
  [[_ spec & predicates] gen]
  (let [bodies (filter seq?
                       (keep #(when (and (seq? %) (= `fn (first %)))
                                (nth % 2 nil))
                             predicates))
        integer-range (some #(when (and (= `s/int-in-range? (first %))
                                        (every? int? (take 2 (rest %)))) %)
                            bodies)
        instant-range (some #(when (and (= `s/inst-in-range? (first %))
                                        (every? inst? (take 2 (rest %)))) %)
                            bodies)
        double-opts (reduce (fn [opts body]
                              (cond
                                (= `<= (first body))
                                (let [[_ a b] body]
                                  (cond
                                    (number? a) (assoc opts :min a)
                                    (number? b) (assoc opts :max b)
                                    :else opts))

                                (= `(not (Double/isNaN ~'%)) body)
                                (assoc opts :nan? false)

                                (= `(not (Double/isInfinite ~'%)) body)
                                (assoc opts :infinity? false)

                                :else opts))
                            {} bodies)
        generator (cond
                    instant-range
                    (let [[_ start end] instant-range]
                      (g/fmap #(java.util.Date. %)
                              (g/long (inst-ms start) (dec (inst-ms end)))))

                    integer-range
                    (integer-generator (cons nil (take 2 (rest integer-range))) gen)

                    (and (= `double? spec) (seq double-opts))
                    (g/double double-opts)

                    :else (gen spec))]
    generator))

(declare multi-spec-generator)

(defn- form-generator
  [f]
  (fn [form {:keys [gen]}]
    (f form gen)))

(defonce ^:private registry-ref
  (atom {:idents
         {`any? (g/one-of
                 [(g/just nil) (g/boolean) (g/long)
                  (g/double) (g/string) (g/keyword)])
          `some? (g/one-of [(g/boolean) (g/long) (g/double)
                            (g/string) (g/keyword)])
          `nil? (g/just nil)
          `boolean? (g/boolean)
          `true? (g/just true)
          `false? (g/just false)
          `string? (g/string)
          `char? (g/fmap char (g/integer 0 65535))
          `integer? (g/long)
          `int? (g/long)
          `pos-int? (g/long {:min 1})
          `neg-int? (g/long {:max -1})
          `nat-int? (g/long {:min 0})
          `zero? (g/just 0)
          `even? (g/fmap #(* 2 %) (g/integer))
          `odd? (g/fmap #(inc (* 2 %)) (g/integer))
          `number? (g/one-of [(g/long) (g/double)])
          `float? (g/one-of [(g/float) (g/double)])
          `double? (g/double)
          `decimal? (g/fmap (fn [[n scale]]
                              (.scaleByPowerOfTen (bigdec n) (int scale)))
                            (g/tuple (g/long) (g/integer -18 18)))
          `rational? (g/one-of
                      [(g/long)
                       (g/fmap #(/ (inc (* 2 %)) 2)
                               (g/long {:min -4611686018427387904
                                        :max 4611686018427387903}))])
          `ratio? (g/fmap #(/ (inc (* 2 %)) 2)
                          (g/long {:min -4611686018427387904
                                   :max 4611686018427387903}))
          `keyword? (g/keyword)
          `simple-keyword? (g/simple-keyword)
          `qualified-keyword? (g/qualified-keyword)
          `symbol? (g/symbol)
          `simple-symbol? (g/simple-symbol)
          `qualified-symbol? (g/qualified-symbol)
          `ident? (g/one-of [(g/keyword) (g/symbol)])
          `simple-ident? (g/one-of [(g/simple-keyword) (g/simple-symbol)])
          `qualified-ident? (g/one-of [(g/qualified-keyword) (g/qualified-symbol)])
          `uuid? (g/fmap (fn [[hi lo]] (UUID. hi lo))
                         (g/tuple (g/long) (g/long)))
          `uri? (g/fmap #(URI. (str "urn:specel:" %)) (g/long))
          `inst? (g/fmap #(Date. (long %)) (g/long))
          `seqable? (g/one-of [(g/just nil) (g/string)
                               (g/vector {:max-size 4} (g/long))
                               (g/list {:max-size 4} (g/long))
                               (g/map {:max-size 4} (g/keyword) (g/long))
                               (g/set {:max-size 4} (g/long))])
          `indexed? (g/vector {:max-size 4} (g/long))
          `map? (g/map {:max-size 4} (g/keyword) (g/long))
          `vector? (g/vector {:max-size 4} (g/long))
          `list? (g/list {:max-size 4} (g/long))
          `seq? (g/list {:max-size 4} (g/long))
          `set? (g/set {:max-size 4} (g/long))
          `coll? (g/one-of [(g/vector {:max-size 4} (g/long))
                            (g/list {:max-size 4} (g/long))
                            (g/map {:max-size 4} (g/keyword) (g/long))
                            (g/set {:max-size 4} (g/long))])
          `empty? (g/one-of (mapv g/just [nil "" [] () {} #{}]))
          `associative? (g/one-of [(g/vector {:max-size 4} (g/long))
                                   (g/map {:max-size 4} (g/keyword) (g/long))])
          `sequential? (g/one-of [(g/vector {:max-size 4} (g/long))
                                  (g/list {:max-size 4} (g/long))])
          `bytes? (g/bytes {:max-size 8})}
         :forms
         (assoc
          (into {} (map (fn [[op f]] [op (form-generator f)]))
                {`s/or (fn [[_ & pairs] gen]
                         (g/one-of (mapv (fn [[tag spec]] (gen spec tag))
                                         (partition 2 pairs))))
                 `s/and and-generator
                 `s/nilable (fn [[_ spec] gen]
                              (g/one-of [(g/just nil) (gen spec ::s/pred)]))
                 `s/tuple (fn [[_ & specs] gen]
                            (g/tuple* (mapv (fn [i spec] (gen spec i)) (range) specs)))
                 `s/keys keys-generator
                 `s/coll-of collection-generator
                 `s/every collection-generator
                 `s/map-of map-generator
                 `s/every-kv map-generator
                 `s/merge (fn [[_ & specs] gen]
                            (g/fmap #(apply merge %) (g/tuple* (mapv gen specs))))
                 `s/int-in integer-generator
                 `s/double-in double-generator})
          `s/multi-spec (fn [form context] (multi-spec-generator form context)))}))

(defonce ^:private default-idents
  (reduce-kv (fn [idents ident generator]
               (if-let [v (and (symbol? ident) (resolve ident))]
                 (assoc idents @v generator)
                 idents))
             (:idents @registry-ref) (:idents @registry-ref)))

(defn registry
  "Returns the generator registry."
  []
  @registry-ref)

(defn ^:no-doc def-impl
  [ident generator]
  (when-not (qualified-ident? ident)
    (throw (ex-info "Expected a namespace-qualified keyword or symbol"
                    {:ident ident})))
  (when-not (instance? Generator generator)
    (throw (ex-info "Expected a Hegel generator" {:ident ident})))
  (swap! registry-ref assoc-in [:idents ident] generator)
  ident)

(defmacro def
  "Given a namespace-qualified keyword or symbol and a Hegel generator,
  makes an entry in the registry mapping ident to the generator."
  [ident generator]
  `(def-impl '~ident ~generator))

(defn ^:no-doc def*-impl
  [ident factory]
  (when-not (qualified-ident? ident)
    (throw (ex-info "Expected a namespace-qualified keyword or symbol"
                    {:ident ident})))
  (when-not (ifn? factory)
    (throw (ex-info "Expected a generator factory" {:ident ident})))
  (swap! registry-ref assoc-in [:idents ident] factory)
  ident)

(defmacro def*
  "Given a namespace-qualified keyword or symbol and a generator factory,
  makes an entry in the registry mapping ident to the factory.
  The factory takes overrides, path and rmap and returns a Hegel generator
  or nil to use default derivation. rmap contains recursion counts and
  :clojure.spec.alpha/recursion-limit. Pass it to gen* to derive generators
  in the same generation context. Explicit gen overrides take precedence."
  [ident factory]
  `(def*-impl '~ident ~factory))

(defn- registered-generator
  [entry registry overrides path ancestors spec]
  (when entry
    (let [generator (if (instance? Generator entry)
                      entry
                      (entry overrides path
                             (with-meta
                               (assoc (merge (into {} ancestors) (::recursion registry))
                                      ::s/recursion-limit (::recursion-limit registry))
                               {::context {:registry registry :ancestors ancestors
                                           :spec (::retag-source (meta spec))
                                           :retag (::retag (meta spec))}})))]
      (when-not (or (nil? generator) (instance? Generator generator))
        (throw (ex-info "Generator factory must return a Hegel generator or nil"
                        {:path path :value generator})))
      generator)))

(declare derive-generator regex-generator resolve-spec)

(defn- tag-spec-ident
  ([spec tag-key]
   (tag-spec-ident spec tag-key #{}))
  ([spec tag-key seen]
   (let [spec (resolve-spec spec)
         seen-next (conj seen spec)
         form (if (s/spec? spec) (s/form spec) spec)]
     (when-not (contains? seen spec)
       (cond
         (and (= predicate-spec-class (class spec))
              (not (captured spec "cpred_QMARK_")))
         (tag-spec-ident (captured spec "pred") tag-key seen-next)

         (and (seq? form) (= `s/keys (first form)))
         (let [{:keys [req opt req-un opt-un]} (apply hash-map (rest form))]
           (or (some #{tag-key} (filter keyword? (flatten (concat req opt))))
               (some #(when (= tag-key (keyword (name %))) %)
                     (filter keyword? (flatten (concat req-un opt-un))))))

         (and (seq? form) (#{`s/and `s/merge} (first form)))
         (some #(tag-spec-ident % tag-key seen-next)
               (if (s/spec? spec) (captured spec "preds") (rest form))))))))

(defn- regex-element-generator
  [spec registry overrides path ancestors]
  (if (s/regex? (resolve-spec spec))
    (derive-generator spec registry overrides path ancestors)
    (g/fmap vector (derive-generator spec registry overrides path ancestors))))

(defn- regex-generator
  [spec registry overrides path ancestors]
  (let [child #(regex-element-generator %1 registry overrides %2 ancestors)]
    (or (when-let [factory (get overrides (::s/name spec))] (factory))
        (case (::s/op spec)
          ::s/accept (g/just (if (= ::s/nil (:ret spec)) [] [(:ret spec)]))
          ::s/amp (if (= ::s/kvs->map (first (:ps spec)))
                    (g/fmap #(vec (mapcat identity %))
                            (derive-generator (second (:ps spec)) registry overrides path ancestors))
                    (child (:p1 spec) path))
          ::s/pcat (g/fmap #(vec (apply concat %))
                           (g/tuple* (mapv (fn [p k]
                                             (child p (if k (conj path k) path)))
                                           (:ps spec) (or (:ks spec) (repeat nil)))))
          ::s/alt (let [generators (into [] (keep (fn [[p k]]
                                                    (productive-generator
                                                     #(child p (if k (conj path k) path)))))
                                         (map vector (:ps spec) (or (:ks spec) (repeat nil))))]
                    (when-not (seq generators)
                      (no-finite-generator spec path))
                    (g/one-of generators))
          ::s/rep (if-let [element (productive-generator #(child (:p2 spec) path))]
                    (g/fmap #(vec (apply concat %)) (g/vector element))
                    (g/just []))
          (throw (ex-info "Unsupported regex op" {:spec spec :path path}))))))

(defn- captured-prefix
  [value prefix]
  (some (fn [field]
          (when (.startsWith (.getName field) prefix)
            (.setAccessible field true)
            (.get field value)))
        (.getDeclaredFields (class value))))

(def ^:private bound-operations
  {`+ + `- - `* * `/ / `inc inc `dec dec `double double `float float
   `long long `int int `min min `max max `bigint bigint `bigdec bigdec})

(defn- bound-value
  [f form]
  (cond
    (or (number? form) (inst? form)) form
    (symbol? form) (or (captured f (munge (name form)))
                       (when (qualified-symbol? form)
                         (some-> (resolve form) deref)))
    (seq? form)
    (when-let [op (get bound-operations (first form))]
      (let [args (mapv #(bound-value f %) (rest form))]
        (when (every? number? args)
          (try
            (apply op args)
            (catch Exception _ nil)))))))

(defn- range-form
  [form preds gfn]
  (letfn [(replace-bounds [pred x]
            (if (seq? x)
              (cond
                (= `s/int-in-range? (first x))
                (list (first x) (bound-value pred (second x))
                      (bound-value pred (nth x 2)) (nth x 3))
                (= `s/inst-in-range? (first x))
                (let [start (bound-value pred (second x))
                      end (bound-value pred (nth x 2))
                      st (when gfn (captured-prefix gfn "st__"))
                      et (when gfn (captured-prefix gfn "et__"))]
                  (list (first x)
                        (or start (when (int? st) (Date. st)))
                        (or end (when (int? et) (Date. et)))
                        (nth x 3)))
                (= `<= (first x))
                (cons `<= (map #(or (bound-value pred %) %) (rest x)))
                :else x)
              x))]
    (cons (first form)
          (map (fn [pred child-form]
                 (if (and (seq? child-form) (= `fn (first child-form))
                          (= 3 (count child-form)))
                   (list `fn (second child-form)
                         (replace-bounds pred (nth child-form 2)))
                   child-form))
               preds (rest form)))))

(defn- draw-value
  [generator]
  (if h/*test-case*
    (h/draw! generator)
    (let [value (volatile! nil)
          result (h/test-fn! {:test-cases 1 :phases [:generate]}
                             (fn [_]
                               (vreset! value [(h/draw! generator)])))]
      (when-not (and (:passed? result) @value)
        (if-let [exception (-> result :failures first :exception)]
          (throw exception)
          (throw (ex-info "Unable to generate function return value" {:result result}))))
      (first @value))))

(defn- spec-generator
  [spec registry overrides path ancestors]
  (let [form (s/form spec)
        op (when (seq? form) (first form))
        child (fn
                ([p] (derive-generator p registry overrides path ancestors))
                ([p k] (derive-generator p registry overrides (conj path k) ancestors)))
        gfn (captured spec "gfn")]
    (cond
      (= predicate-spec-class (class spec))
      (if (captured spec "cpred_QMARK_")
        (throw (ex-info "Conformer requires a generator override" {:spec form :path path}))
        (derive-generator (captured spec "pred") registry (dissoc overrides path) path ancestors))
      (captured spec "re") (regex-generator (captured spec "re") registry overrides path ancestors)
      (= `s/or op)
      (let [generators (into [] (keep (fn [[p k]]
                                        (productive-generator #(child p k))))
                             (map vector (captured spec "preds") (captured spec "keys")))]
        (when-not (seq generators)
          (no-finite-generator form path))
        (g/one-of generators))
      (= `s/keys op)
      (keys-generator form child)
      (= `s/tuple op) (g/tuple* (mapv (fn [p k] (child p k))
                                      (captured spec "preds") (range)))
      (= `s/nilable op)
      (if-let [generator (productive-generator #(child (captured spec "pred") ::s/pred))]
        (g/one-of [(g/just nil) generator])
        (g/just nil))
      (= `s/nonconforming op) (child @(captured spec "spec"))
      (= `s/merge op) (g/fmap #(apply merge %) (g/tuple* (mapv child (captured spec "preds"))))
      (#{`s/map-of `s/every-kv} op)
      (let [preds (captured (captured spec "pred") "preds")
            opts (captured spec "opts")
            kind (:kind opts)
            opts (cond-> opts
                   (identical? kind map?) (assoc :kind `map?)
                   ;; every-kv injects :into {}, even for a nonmap kind.
                   ;; Only an explicit :into should override that kind's family.
                   (and kind (not (identical? kind map?))
                        (not (some #{:into} (take-nth 2 (drop 3 form)))))
                   (dissoc :into))]
        (map-generator (concat [`s/every-kv (first preds) (second preds)]
                               (mapcat identity opts)) child))
      (#{`s/coll-of `s/every} op)
      (collection-generator (concat [op (captured spec "pred")]
                                    (mapcat identity (captured spec "opts"))) child)
      (= `s/and op)
      (let [preds (captured spec "preds")
            factory (or (get overrides (first preds)) (get overrides (second form)))
            overridden (when factory (factory))
            _ (when-not (or (nil? overridden) (instance? Generator overridden))
                (throw (ex-info "Override must return a Hegel generator"
                                {:spec (first preds) :path path})))
            child-overrides (dissoc overrides (first preds) (second form) path)
            first-child #(derive-generator (first preds) registry child-overrides path ancestors)
            entry (get-in registry [:idents (first preds)])
            registered (when (and (nil? overridden) entry
                                  (not (identical? entry (get default-idents (first preds)))))
                         (registered-generator entry registry overrides path ancestors
                                               (resolve-spec (first preds))))
            generator (or overridden registered
                          (if (= (count preds) 1)
                            (first-child)
                            (and-generator (range-form form preds gfn)
                                           (fn [p]
                                             (if (= p (second form))
                                               (first-child)
                                               (child p))))))]
        (filter-generator #(valid-candidate? spec %) generator))
      (= `s/conformer op)
      (throw (ex-info "Conformer requires a generator override" {:spec form :path path}))
      (= `s/multi-spec op)
      (child (list op (captured spec "mmvar") (captured spec "retag")))
      (= `s/fspec op)
      (let [args (:args spec)
            ret (:ret spec)
            relation (:fn spec)
            registry (dissoc registry ::recursion)]
        (g/just
         (fn [& values]
           (let [conformed-args (s/conform args values)]
             (when (s/invalid? conformed-args)
               (throw (ex-info "Arguments do not satisfy function spec"
                               {:args values :spec (s/form args)})))
             (let [result-generator (derive-generator ret registry overrides [] {})]
               (draw-value
                (if relation
                  (filter-generator #(s/valid? relation
                                               {:args conformed-args
                                                :ret (s/conform ret %)})
                                    result-generator)
                  result-generator)))))))
      (and (captured spec "pred")
           (or (set? (captured spec "pred")) (not (seq? form))))
      (derive-generator (captured spec "pred") registry (dissoc overrides path) path ancestors)
      :else (child form))))

(defn- multi-spec-generator
  [[_ mm-source retag] {:keys [registry overrides path ancestors]}]
  (let [mm (cond
             (instance? clojure.lang.MultiFn mm-source) mm-source
             (instance? clojure.lang.IDeref mm-source) @mm-source
             (symbol? mm-source) (some-> (resolve mm-source) deref))]
    (when-not (instance? clojure.lang.MultiFn mm)
      (throw (ex-info "Expected a multimethod" {:multimethod mm-source :path path})))
    (let [id mm
          registry (enter-branch registry id)
          default-dispatch (.defaultDispatchVal ^clojure.lang.MultiFn mm)
          tags (vec (remove #(or (= default-dispatch %)
                                 (= ::s/invalid %)
                                 (recursion-limit? registry id path %))
                            (keys (methods mm))))
          tag-ident (when (keyword? retag)
                      (or (when (qualified-keyword? retag) retag)
                          (some #(tag-spec-ident ((get-method mm %) nil) retag) tags)))
          selector (or (get overrides (conj path ::dispatch))
                       (when (keyword? retag)
                         (or (get overrides tag-ident)
                             (get overrides (conj path retag)))))
          branch-generator
          (fn [tag]
            (productive-generator
             (fn []
               (let [method (get-method mm tag)]
                 (when-not method
                   (throw (ex-info "No multi-spec method for dispatch value"
                                   {:multimethod mm-source :dispatch tag :path path})))
                 (let [branch (method nil)
                       branch (if (or (s/spec? branch) (s/regex? branch))
                                (vary-meta branch assoc ::retag-source branch ::retag
                                           (fn [value]
                                             (if (keyword? retag)
                                               (assoc value retag tag)
                                               (retag value tag))))
                                branch)
                       branch-overrides (if (keyword? retag)
                                          (assoc overrides (or (tag-spec-ident branch retag) retag)
                                                 #(g/just tag))
                                          overrides)
                       generator (derive-generator branch registry branch-overrides
                                                   (conj path tag) ancestors)]
                   (g/fmap (fn [value]
                             (if (keyword? retag)
                               (assoc value retag tag)
                               (retag value tag)))
                           generator))))))
          generators (into {} (keep (fn [tag]
                                      (when-let [generator (branch-generator tag)]
                                        [tag generator])))
                           tags)
          tag-generator (if selector
                          (selector)
                          (or (when (keyword? retag)
                                (registered-generator (get-in registry [:idents tag-ident])
                                                      registry overrides (conj path retag) ancestors nil))
                              (when-let [productive-tags (seq (filter #(contains? generators %) tags))]
                                (g/sampled-from (vec productive-tags)))))]
      (when-not (seq generators)
        (no-finite-generator (list `s/multi-spec mm-source retag) path))
      (when-not (instance? Generator tag-generator)
        (throw (ex-info "No dispatch generator for multi-spec"
                        {:multimethod mm-source :path path})))
      (g/bind
       (fn [tag]
         (when (or (= default-dispatch tag)
                   (= ::s/invalid tag)
                   (recursion-limit? registry id path tag))
           (throw (AssumeRejected.)))
         (or (get generators tag)
             (when-not (contains? (methods mm) tag)
               (branch-generator tag))
             (throw (AssumeRejected.))))
       tag-generator))))

(defn- resolve-spec
  [spec]
  (loop [spec spec
         seen #{}]
    (if (and (qualified-ident? spec) (not (contains? seen spec)))
      (if-let [registered (s/get-spec spec)]
        (recur registered (conj seen spec))
        spec)
      spec)))

(defn- alias-override
  [spec overrides]
  (let [registry (s/registry)]
    (loop [ident spec
           seen #{}]
      (when (and (qualified-ident? ident) (not (contains? seen ident)))
        (or (get overrides ident)
            (recur (get registry ident) (conj seen ident)))))))

(defn- derive-generator
  ([spec registry overrides path ancestors]
   (derive-generator spec registry overrides path ancestors false))
  ([spec {:keys [idents forms] :as registry} overrides path ancestors direct?]
   (let [original spec
         spec (resolve-spec spec)
         ident (or (::s/name (meta spec)) (when (map? spec) (::s/name spec)) spec)
         override (if (s/regex? spec)
                    (or (get overrides path) (alias-override original overrides))
                    (or (get overrides ident) (alias-override original overrides)
                        (get overrides path)))
         generator
         (or (when override
               (let [generator (override)]
                 (when-not (or (nil? generator) (instance? Generator generator))
                   (throw (ex-info "Override must return a Hegel generator"
                                   {:spec spec :path path})))
                 generator))
             (when-not direct?
               (registered-generator (or (get idents original) (get idents ident))
                                     registry overrides path ancestors spec))
             (when-not direct? (get default-idents original))
             (when-not direct? (get default-idents ident))
             (let [ancestors (enter-spec spec registry overrides path ancestors)]
               (cond
                 (qualified-keyword? spec)
                 (if-let [registered (s/get-spec spec)]
                   (derive-generator registered registry overrides path ancestors)
                   (throw (ex-info "Spec is not registered" {:spec spec :path path})))

                 (s/regex? spec)
                 (regex-generator spec registry overrides path ancestors)

                 (s/spec? spec)
                 (spec-generator spec registry overrides path ancestors)

                 (set? spec)
                 (if (seq spec)
                   (g/one-of (mapv g/just spec))
                   (throw (ex-info "Cannot generate from an empty set"
                                   {:spec spec :path path})))

                 (seq? spec)
                 (if-let [factory (get forms (first spec))]
                   (factory spec
                            {:registry registry :overrides overrides
                             :path path :ancestors ancestors
                             :gen (fn
                                    ([child]
                                     (derive-generator child registry overrides path ancestors))
                                    ([child segment]
                                     (derive-generator child registry overrides
                                                       (conj path segment) ancestors)))})
                   (throw (ex-info "No generator for spec form" {:spec spec :path path})))

                 :else
                 (throw (ex-info "No generator for spec; register one with specel.core/def"
                                 {:spec spec :path path})))))]
     (if (or (s/spec? spec) (s/regex? spec) (qualified-keyword? spec))
       (filter-generator #(valid-candidate? spec %) generator)
       generator))))

(defn- normalize-spec
  [spec]
  (when (seq? spec)
    (throw (ex-info "Expected a spec, not an unevaluated form" {:spec spec})))
  (if (var? spec)
    (if (s/get-spec spec)
      (symbol (str (ns-name (:ns (meta spec)))) (str (:name (meta spec))))
      @spec)
    spec))

(defn- normalize-overrides
  [overrides]
  (reduce-kv (fn [m ident factory]
               (if-let [v (and (symbol? ident) (resolve ident))]
                 (assoc m @v factory)
                 m))
             (or overrides {}) (or overrides {})))

(defn- generate
  [spec overrides path registry ancestors direct?]
  (let [spec (normalize-spec spec)
        overrides (normalize-overrides overrides)
        predicate (if (symbol? spec)
                    (or (s/get-spec spec)
                        (some-> (resolve spec) deref)
                        (throw (ex-info "Cannot resolve predicate" {:spec spec})))
                    spec)]
    (filter-generator #(valid-candidate? predicate %)
                      (derive-generator spec registry overrides path ancestors
                                        (and direct? (or (s/spec? spec) (s/regex? spec)))))))

(defn gen*
  "Returns a Hegel generator in the context supplied to a def* factory.
  Overrides and path may be changed to derive a subtree. Pass the factory's
  rmap with its metadata intact; it captures the generation context and may
  be retained for delayed generation. A direct spec or regex object derives
  its implementation without invoking its own name registration, while a
  named reference dispatches through the registry normally."
  [spec overrides path rmap]
  (let [{:keys [registry ancestors retag] context-spec :spec} (::context (meta rmap))
        spec (normalize-spec spec)
        spec (if (and retag (identical? spec context-spec))
               (vary-meta spec assoc ::retag retag ::retag-source context-spec)
               spec)]
    (when-not registry
      (throw (ex-info "Expected the rmap supplied to a generator factory" {:rmap rmap})))
    (generate spec overrides path registry ancestors true)))

(defn gen
  "Given a spec, returns the Hegel generator for it, or throws if none can
  be constructed. Optionally an overrides map can be provided which
  should map spec names or paths to no-arg generator-creating fns.
  These will be used instead of the generators at those names/paths.
  Overrides can name any identifier in a direct alias chain. For regular
  specs, the resolved name takes precedence, followed by aliases from
  outermost to innermost, then the path. A parent generator supersedes
  those of any subtrees."
  ([spec]
   (gen spec nil))
  ([spec overrides]
   (let [registry (update (assoc (registry) ::recursion-limit *recursion-limit*) :idents
                          (fn [idents]
                            (reduce-kv (fn [m ident generator]
                                         (if-let [v (and (symbol? ident) (resolve ident))]
                                           (assoc m @v generator)
                                           m))
                                       idents idents)))]
     (generate spec overrides [] registry {} false))))
