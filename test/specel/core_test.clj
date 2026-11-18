(ns specel.core-test
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sg]
            [clojure.test :refer [deftest is]]
            [hegel-clj.clojure-test :refer [with]]
            [hegel-clj.core :as h]
            [hegel-clj.generator :as g]
            [specel.core :as sc])
  (:import (dev.hegel Generator)))

(defn- leaf-form-generator
  []
  (g/one-of
   [(g/sampled-from [`any? `nil? `boolean? `true? `false? `string? `char?
                     `integer? `int? `pos-int? `neg-int? `nat-int?
                     `number? `float? `double? `keyword?
                     `simple-keyword? `qualified-keyword? `symbol?
                     `simple-symbol? `qualified-symbol? `ident?
                     `simple-ident? `qualified-ident? `uuid?])
    (g/sampled-from [`(s/and int? even?) `(s/and int? odd?)
                     `(s/and #{0} zero?)])
    (g/set {:min-size 1 :max-size 5} (g/integer -10 10))
    (g/fmap (fn [[start width]] `(s/int-in ~start ~(+ start width)))
            (g/tuple (g/integer -100 100) (g/integer 1 100)))
    (g/fmap (fn [[start width]]
              `(s/double-in :min ~(double start)
                            :max ~(double (+ start width))
                            :NaN? false :infinite? false))
            (g/tuple (g/integer -100 100) (g/integer 1 100)))]))

(defn- spec-form-generator
  [depth]
  (if (zero? depth)
    (leaf-form-generator)
    (let [child (spec-form-generator (dec depth))]
      (g/one-of
       [(leaf-form-generator)
        (g/fmap (fn [form] `(s/nilable ~form)) child)
        (g/fmap (fn [forms] `(s/tuple ~@forms))
                (g/vector {:min-size 1 :max-size 3} child))
        (g/fmap (fn [[a b]] `(s/or :a ~a :b ~b)) (g/tuple child child))
        (g/fmap (fn [form] `(s/and ~form (constantly true))) child)
        (g/fmap (fn [[form kind size distinct?]]
                  `(s/coll-of ~form :kind ~kind :max-count ~size
                              :distinct ~distinct?))
                (g/tuple child (g/sampled-from [`vector? `list? `set?])
                         (g/integer 0 4) (g/boolean)))
        (g/fmap (fn [[k v size]] `(s/map-of ~k ~v :max-count ~size))
                (g/tuple (g/sampled-from [`keyword? `int?])
                         child (g/integer 0 4)))]))))

(deftest generated-specs-test
  (doseq [seed [42 137 2026]]
    (let [result
          (with {:test-cases 1000 :seed seed :database nil}
                [form (spec-form-generator 3)]
                (let [spec (binding [*ns* (the-ns 'specel.core-test)]
                             (eval `(s/spec ~form)))
                      generator (sc/gen spec)]
                  (is (instance? Generator generator) (pr-str form))
                  (let [value (h/draw! generator)]
                    (is (s/valid? spec value)
                        (pr-str {:form form :value value})))))]
      (is (:passed? result) (pr-str result))
      (is (<= 1000 (get-in result [:statistics :valid])) (pr-str result)))))

(deftest constrained-specs-test
  (let [result
        (with {:test-cases 300 :seed 44 :database nil}
              [start (g/integer -1000000000 1000000000)
               width (g/integer 1 3)
               length (g/integer 20 40)
               parity (g/sampled-from [`even? `odd?])]
              (let [form `(s/tuple
                           (s/int-in ~start ~(+ start width))
                           (s/double-in :min ~(double start)
                                        :max ~(+ (double start) 0.00001)
                                        :NaN? false :infinite? false)
                           (s/tuple ~@(repeat length `(s/and int? ~parity))))
                    spec (eval form)
                    generator (sc/gen spec)
                    value (h/draw! generator)]
                (is (instance? Generator generator))
                (is (s/valid? spec value) (pr-str {:form form :value value}))))]
    (is (:passed? result) (pr-str result))
    (is (<= 300 (get-in result [:statistics :valid])) (pr-str result))))

(deftest registered-generators-test
  (let [result
        (with {:test-cases 200 :seed 43 :database nil}
              [value (g/keyword)
               override (g/keyword)]
              (s/def ::registered keyword?)
              (s/def ::alias ::registered)
              (is (= ::registered (sc/def ::registered (g/just value))))
              (let [spec (eval `(s/keys :req [::alias] :opt-un [::registered]))
                    result (h/draw! (sc/gen spec))]
                (is (s/valid? spec result))
                (is (= value (::alias result))))
              (let [spec (eval `(s/tuple ::alias ::registered))
                    result (h/draw! (sc/gen spec {::registered #(g/just override)}))]
                (is (s/valid? spec result))
                (is (= [override override] result))))]
    (is (:passed? result) (pr-str result))
    (is (<= 200 (get-in result [:statistics :valid])) (pr-str result))))

(deftest alias-override-precedence-test
  (let [result
        (with {:test-cases 300 :seed 67 :database nil}
              [depth (g/integer 1 4)
               value (g/integer -1000 1000)
               regex? (g/boolean)
               nested? (g/boolean)
               named? (g/boolean)
               parent? (g/boolean)]
              (let [form (if regex? `(s/* integer?) `integer?)
                    target ::override-target
                    aliases (mapv #(keyword (str (ns-name *ns*)) (str "override-alias-" %))
                                  (range depth))
                    payload (if regex? [value] value)
                    path-value (if regex? [(inc value)] (inc value))
                    alias-value (if regex? [(+ 2 value)] (+ 2 value))]
                (eval `(s/def ~target ~form))
                (doseq [[alias previous] (map vector aliases (cons target aliases))]
                  (eval `(s/def ~alias ~previous)))
                (let [alias (peek aliases)
                      spec (if nested? (eval `(s/tuple ~alias)) alias)
                      path (if nested? [0] [])
                      parent-value [path-value]
                      entries (cond-> {path path-value}
                                named? (assoc target payload)
                                (and nested? parent?) (assoc [] parent-value))
                      alias-entries (merge (zipmap aliases (repeat alias-value)) entries)
                      overrides (fn [just entries]
                                  (into {} (map (fn [[k v]] [k #(just v)])) entries))
                      native (sg/generate (s/gen spec (overrides sg/return entries)))
                      generated (h/draw! (sc/gen spec (overrides g/just entries)))
                      alias-generated (h/draw! (sc/gen spec (overrides g/just alias-entries)))
                      child (cond regex? path-value named? payload :else alias-value)
                      expected (cond
                                 (and nested? parent?) parent-value
                                 nested? [child]
                                 :else child)]
                  (is (s/valid? spec generated))
                  (is (= native generated)
                      (pr-str {:entries entries :spec spec :native native :value generated}))
                  (is (s/valid? spec alias-generated))
                  (is (= expected alias-generated)
                      (pr-str {:entries alias-entries :spec spec :expected expected :value alias-generated})))))]
    (is (:passed? result) (pr-str result))
    (is (<= 300 (get-in result [:statistics :valid])) (pr-str result))))

(deftest filter-budget-test
  (let [result
        (with {:test-cases 100 :seed 45 :database nil}
              [threshold (g/integer 4 100)
               value (g/integer)]
              (let [form `(let [attempts# (atom 0)]
                            (s/and integer?
                                   (fn [_#] (<= ~threshold (swap! attempts# inc)))))
                    spec (eval form)
                    generator (sc/gen spec {`integer? #(g/just value)})
                    samples (h/sample {:test-cases 1 :database nil} generator)]
                (is (= [value] samples))
                (is (every? #(s/valid? spec %) samples))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest sampling-failures-test
  (let [result
        (with {:test-cases 30 :seed 46 :database nil}
              [value (g/integer)
               throwing? (g/boolean)]
              (let [form (if throwing?
                           `(s/and integer? (fn [n#] (= ~value (last n#))))
                           `(s/and integer? (fn [_#] false)))
                    spec (eval form)
                    generator (sc/gen spec)
                    report (h/test! {:test-cases 1 :database nil}
                                    (h/draw! generator))]
                (is (false? (:passed? report)))
                (if throwing?
                  (is (instance? IllegalArgumentException
                                 (-> report :failures first :exception)))
                  (is (zero? (get-in report [:statistics :valid]))))))]
    (is (:passed? result) (pr-str result))
    (is (<= 30 (get-in result [:statistics :valid])) (pr-str result))))

(s/def ::kind keyword?)

(defn- multi-spec-form-generator
  [child]
  (g/fmap
   (fn [[a b]]
     (let [mm (symbol "specel.core-test" (str (gensym "event")))
           a-key (keyword "specel.core-test" (str (gensym "a")))
           b-key (keyword "specel.core-test" (str (gensym "b")))]
       {:form `(do
                 (defmulti ~mm ::kind)
                 (s/def ~a-key ~a)
                 (s/def ~b-key ~b)
                 (defmethod ~mm :a [_#]
                   (s/and (s/keys :req [::kind ~a-key])
                          (fn [m#] (= :a (::kind m#)))))
                 (defmethod ~mm :b [_#]
                   (s/and (s/keys :req [::kind ~b-key])
                          (fn [m#] (= :b (::kind m#)))))
                 (s/multi-spec ~mm ::kind))
        :fields {:a a-key :b b-key}}))
   (g/tuple child child)))

(deftest generated-multi-specs-test
  (let [result
        (with {:test-cases 200 :seed 47 :database nil}
              [description (multi-spec-form-generator (spec-form-generator 2))]
              (let [spec (binding [*ns* (the-ns 'specel.core-test)]
                           (eval (:form description)))
                    generator (sc/gen spec)
                    value (h/draw! generator)]
                (is (instance? Generator generator))
                (is (s/valid? spec value) (pr-str {:form (:form description) :value value}))))]
    (is (:passed? result) (pr-str result))
    (is (<= 200 (get-in result [:statistics :valid])) (pr-str result))))

(deftest multi-spec-overrides-test
  (let [result
        (with {:test-cases 100 :seed 48 :database nil}
              [description (multi-spec-form-generator (g/just `integer?))
               tag (g/sampled-from [:a :b])
               expected (g/integer)
               global? (g/boolean)]
              (let [spec (binding [*ns* (the-ns 'specel.core-test)]
                           (eval `(do ~@(butlast (rest (:form description)))
                                      (s/tuple ~(last (:form description))))))
                    field (get (:fields description) tag)
                    tag-location (if global? ::kind [0 ::kind])
                    value (h/draw! (sc/gen spec
                                           {tag-location #(g/just tag)
                                            [0 tag field] #(g/just expected)}))]
                (is (s/valid? spec value))
                (is (= tag (::kind (first value))))
                (is (= expected (get (first value) field)))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest path-overrides-test
  (let [result
        (with {:test-cases 100 :seed 49 :database nil}
              [a (g/integer)
               b (g/integer)
               global? (g/boolean)]
              (s/def ::payload integer?)
              (let [spec (eval `(s/tuple (s/keys :req-un [::payload])
                                         (s/keys :req-un [::payload])))
                    value (h/draw! (sc/gen spec
                                           (cond-> {[0 :payload] #(g/just a)
                                                    [1 :payload] #(g/just b)}
                                             global? (assoc ::payload #(g/just a)))))]
                (is (s/valid? spec value))
                (is (= a (:payload (first value))))
                (is (= (if global? a b) (:payload (second value))))
                (is (= [{:payload a} {:payload b}]
                       (h/draw! (sc/gen spec
                                        {[] #(g/just [{:payload a} {:payload b}])
                                         [0 :payload] #(throw (ex-info "Unused child override" {}))}))))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest multi-spec-retag-test
  (let [result
        (with {:test-cases 100 :seed 50 :database nil}
              [tag (g/sampled-from [:a :b])
               expected (g/integer)
               mode (g/sampled-from [:unqualified :function])]
              (let [mm (symbol "specel.core-test" (str (gensym "retag")))
                    retag (if (= :function mode)
                            `(fn [m# tag#] (assoc m# :kind tag#))
                            :kind)
                    form `(do
                            (s/def ::amount integer?)
                            (defmulti ~mm :kind)
                            (defmethod ~mm :a [_#]
                              (s/and (s/keys :req-un [::kind ::amount])
                                     (fn [m#] (= :a (:kind m#)))))
                            (defmethod ~mm :b [_#]
                              (s/and (s/keys :req-un [::kind ::amount])
                                     (fn [m#] (= :b (:kind m#)))))
                            (s/multi-spec ~mm ~retag))
                    spec (binding [*ns* (the-ns 'specel.core-test)] (eval form))
                    selector (if (= :function mode) [::sc/dispatch] ::kind)
                    value (h/draw! (sc/gen spec
                                           {selector #(g/just tag)
                                            [tag :amount] #(g/just expected)}))]
                (is (s/valid? spec value))
                (is (= tag (:kind value)))
                (is (= expected (:amount value)))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(defn- regex-form-generator
  [depth]
  (let [leaf (g/sampled-from [`integer? `string? `keyword?])]
    (if (zero? depth)
      leaf
      (let [child (regex-form-generator (dec depth))]
        (g/one-of
         [leaf
          (g/fmap (fn [form] `(s/? ~form)) child)
          (g/fmap (fn [form] `(s/* (s/spec ~form))) child)
          (g/fmap (fn [form] `(s/+ (s/spec ~form))) child)
          (g/fmap (fn [[a b]] `(s/cat :a ~a :b ~b)) (g/tuple child child))
          (g/fmap (fn [[a b]] `(s/alt :a ~a :b ~b)) (g/tuple child child))
          (g/fmap (fn [form] `(s/& (s/* (s/spec ~form)) (fn [xs#] (<= (count xs#) 5)))) child)
          (g/fmap (fn [form] `(s/spec (s/cat :nested ~form))) child)])))))

(deftest built-in-regex-specs-test
  (let [result
        (with {:test-cases 500 :seed 51 :database nil}
              [form (regex-form-generator 3)]
              (let [spec (eval `(s/spec ~form))
                    generator (sc/gen spec)
                    value (h/draw! generator)]
                (is (instance? Generator generator))
                (is (s/valid? spec value) (pr-str {:form form :value value}))))]
    (is (:passed? result) (pr-str result))
    (is (<= 500 (get-in result [:statistics :valid])) (pr-str result))))

(deftest built-in-predicates-test
  (doseq [predicate [`any? `some? `number? `integer? `int? `pos-int? `neg-int?
                     `nat-int? `float? `double? `boolean? `string? `ident?
                     `simple-ident? `qualified-ident? `keyword? `simple-keyword?
                     `qualified-keyword? `symbol? `simple-symbol? `qualified-symbol?
                     `uuid? `uri? `decimal? `inst? `seqable? `indexed? `map?
                     `vector? `list? `seq? `char? `set? `nil? `false? `true?
                     `zero? `rational? `coll? `empty? `associative? `sequential?
                     `ratio? `bytes?]]
    (let [result
          (with {:test-cases 20 :seed 52 :database nil}
                [spec (g/just (eval `(s/spec ~predicate)))]
                (is (s/valid? spec (h/draw! (sc/gen spec))) (str predicate)))]
      (is (:passed? result) (pr-str {:predicate predicate :result result}))
      (is (pos? (get-in result [:statistics :valid])) (pr-str result)))))

(deftest decimal-generator-test
  (let [result (with {:test-cases 1 :seed 52 :database nil}
                     [values (g/vector {:min-size 100 :max-size 100} (sc/gen decimal?))]
                     (is (every? decimal? values))
                     (is (some #(not (zero? (rem % 1M))) values))
                     (is (some pos? values))
                     (is (some neg? values)))]
    (is (:passed? result) (pr-str result))))

(deftest built-in-wrappers-test
  (let [result
        (with {:test-cases 200 :seed 53 :database nil}
              [n (g/integer -1000000 1000000)
               form (g/sampled-from
                     [`(s/nonconforming (s/or :n integer? :s string?))
                      `(s/and (s/or :n integer?) map-entry?)
                      `(s/merge (s/keys :req [::left]) (s/keys :req [::right]))
                      `(s/keys :req [(or ::left ::right)])
                      `(s/keys :req-un [(and ::left ::right)])
                      `(s/keys* :req-un [::left] :opt [::right])
                      `(s/cat :first integer? :entries (s/keys* :req [::left]))
                      `(s/every integer? :kind list? :count 3)
                      `(s/every-kv keyword? integer? :max-count 3)
                      `(s/coll-of integer? :kind vector? :into [99] :count 2)])]
              (s/def ::left integer?)
              (s/def ::right string?)
              (let [spec (eval form)
                    value (h/draw! (sc/gen spec))]
                (is (s/valid? spec value) (pr-str {:form form :value value})))
              (let [spec (eval `(s/inst-in (java.util.Date. ~n) (java.util.Date. ~(inc n))))
                    value (h/draw! (sc/gen spec))]
                (is (s/valid? spec value))
                (is (= n (inst-ms value)))))]
    (is (:passed? result) (pr-str result))
    (is (<= 200 (get-in result [:statistics :valid])) (pr-str result))))

(deftest custom-generators-test
  (let [result
        (with {:test-cases 100 :seed 54 :database nil}
              [n (g/integer)]
              (let [spec (eval `(s/tuple
                                 (s/conformer (fn [x#] (if (integer? x#) (str x#) ::s/invalid)))
                                 (s/spec (fn [x#] (= ~n x#)))))
                    value (h/draw! (sc/gen spec {[0] (fn [] (g/just n))
                                                 [1] (fn [] (g/just n))}))]
                (is (= [n n] value))
                (is (s/valid? spec value))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest function-specs-test
  (let [result
        (with {:test-cases 30 :seed 55 :database nil}
              [n (g/integer)]
              (let [spec (eval `(s/fspec :args (s/cat :x integer?) :ret #{~n}))
                    f (h/draw! (sc/gen spec))]
                (is (s/valid? spec f))
                (is (= n (f n)))))]
    (is (:passed? result) (pr-str result))
    (is (<= 30 (get-in result [:statistics :valid])) (pr-str result))))

(deftest spec-attached-generators-ignored-test
  (let [result
        (with {:test-cases 100 :seed 56 :database nil}
              [n (g/integer)]
              (doseq [form [`(s/with-gen (s/and integer? (constantly true))
                               (fn [] (throw (ex-info "Must not invoke spec generator" {}))))
                            `(s/with-gen (s/cat :n integer?)
                               (fn [] (throw (ex-info "Must not invoke spec generator" {}))))
                            `(s/spec integer?
                                     :gen (fn [] (throw (ex-info "Must not invoke spec generator" {}))))]]
                (let [spec (eval form)
                      value (h/draw! (sc/gen spec))]
                  (is (s/valid? spec value)))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest registered-function-specs-test
  (let [result
        (with {:test-cases 30 :seed 57 :database nil}
              [n (g/integer)]
              (let [sym (symbol "specel.core-test" (str (gensym "function")))
                    v (intern 'specel.core-test (symbol (name sym)) identity)
                    spec (binding [*ns* (the-ns 'specel.core-test)]
                           (eval `(do (s/fdef ~sym :args (s/cat :n integer?) :ret #{~n})
                                      (s/get-spec '~sym))))]
                (doseq [input [sym v spec]]
                  (let [f (h/draw! (sc/gen input))]
                    (is (s/valid? spec f))
                    (is (= n (f n)))))))]
    (is (:passed? result) (pr-str result))
    (is (<= 30 (get-in result [:statistics :valid])) (pr-str result))))

(deftest recursive-specs-test
  (let [result
        (with {:test-cases 100 :seed 58 :database nil}
              [leaf (g/sampled-from [`integer? `string? `keyword?])]
              (let [ident (keyword "specel.core-test" (str (gensym "tree")))
                    spec (eval `(do (s/def ~ident (s/or :leaf ~leaf
                                                        :branch (s/tuple ~ident ~ident)))
                                    (s/get-spec ~ident)))
                    value (h/draw! (sc/gen spec))]
                (is (s/valid? spec value))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest set-and-conformer-specs-test
  (let [result
        (with {:test-cases 100 :seed 59 :database nil}
              [n (g/integer)]
              (let [spec (eval `(s/spec #{nil false ~n}))
                    value (h/draw! (sc/gen spec))]
                (is (s/valid? spec value)))
              (let [spec (eval `(s/conformer (fn [x#] (if (= ~n x#) (str x#) ::s/invalid))
                                             (fn [x#] (Long/parseLong x#))))
                    value (h/draw! (sc/gen spec {[] #(g/just n)}))]
                (is (s/valid? spec value))
                (is (= n (s/unform spec (s/conform spec value)))))
              (is (thrown? clojure.lang.ExceptionInfo
                           (sc/gen (s/conformer identity))))
              (let [spec (s/fspec :args (s/cat :n integer?) :ret integer?
                                  :fn #(= (get-in % [:args :n]) (:ret %)))]

                (is (s/valid? spec (h/draw! (sc/gen spec {[] #(g/just identity)}))))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest named-regex-and-runtime-bounds-test
  (let [result
        (with {:test-cases 100 :seed 60 :database nil}
              [lo (g/integer 1000000000 1000000100)
               n (g/integer)]
              (s/def ::pair (s/cat :a integer? :b integer?))
              (doseq [spec [(s/cat :pair ::pair) (s/+ ::pair)
                            (s/int-in lo (inc lo))
                            (s/double-in :min (double lo) :max (+ (double lo) 0.00001))]]
                (let [value (h/draw! (sc/gen spec))]
                  (is (s/valid? spec value))))
              (is (= [0 0]
                     (h/draw! (sc/gen (s/get-spec ::pair)
                                      {::pair #(g/just [n n])
                                       [] #(g/just [0 0])})))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest recursive-path-overrides-test
  (let [result
        (with {:test-cases 100 :seed 61 :database nil}
              [n (g/integer)]
              (s/def ::recursive (s/or :leaf integer? :branch (s/tuple ::recursive)))
              (let [spec (s/get-spec ::recursive)
                    value (h/draw! (sc/gen spec
                                           {[:leaf] #(g/just n)
                                            [:branch 0] #(g/just n)}))]
                (is (s/valid? spec value))
                (is (or (= n value) (= [n] value))))
              (let [spec (s/get-spec ::recursive)
                    value (h/draw! (sc/gen spec
                                           {[:leaf] #(g/just n)
                                            [:branch 0 :leaf] #(g/just n)
                                            [:branch 0 :branch] #(g/just [n])}))]
                (is (s/valid? spec value))
                (is (contains? #{n [n] [[n]]} value))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest function-invocations-test
  (let [result
        (with {:test-cases 100 :seed 62 :database nil}
              [start (g/integer -1000 1000)
               calls (g/integer 2 10)
               outside? (g/boolean)]
              (let [spec (eval `(s/fspec :args (s/cat :n integer?) :ret integer?))
                    draws (atom start)
                    return-generator (g/composite (swap! draws inc))
                    f (h/draw! (sc/gen spec {integer? #(identity return-generator)}))]
                (reset! draws start)
                (let [values (if outside?
                               (binding [h/*test-case* nil]
                                 (mapv f (range calls)))
                               (mapv f (range calls)))]
                  (is (= (vec (range (inc start) (+ start calls 1))) values))
                  (is (= (+ start calls) @draws)))
                (is (thrown? clojure.lang.ExceptionInfo (f :invalid)))
                (is (= (+ start calls) @draws))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest function-return-failures-test
  (let [result
        (with {:test-cases 30 :seed 63 :database nil}
              [n (g/integer)]
              (let [spec (eval `(s/fspec :args (s/cat :n integer?) :ret integer?))
                    fail? (atom false)
                    return-generator (reify Generator
                                       (doDraw [_ _]
                                         (if @fail?
                                           (throw (ex-info "Return generation failed" {:n n}))
                                           n)))
                    f (h/draw! (sc/gen spec {integer? #(identity return-generator)}))]
                (reset! fail? true)
                (let [exception (try
                                  (binding [h/*test-case* nil] (f n))
                                  nil
                                  (catch clojure.lang.ExceptionInfo e e))]
                  (is (= "Return generation failed" (ex-message exception)))
                  (is (= {:n n} (ex-data exception))))))]
    (is (:passed? result) (pr-str result))
    (is (<= 30 (get-in result [:statistics :valid])) (pr-str result))))

(deftest function-replay-test
  (let [result
        (with {:test-cases 30 :seed 64 :database nil}
              [seed (g/long 0 1000000)
               calls (g/integer 2 10)]
              (let [spec (s/fspec :args (s/cat :n integer?) :ret integer?)
                    generator (sc/gen spec)
                    run (fn []
                          (let [values (atom [])
                                report (h/test-fn! {:test-cases 1 :seed seed :phases [:generate]}
                                                   (fn [_]
                                                     (let [f (h/draw! generator)]
                                                       (reset! values (mapv f (range calls))))))]
                            (is (:passed? report) (pr-str report))
                            @values))
                    first-run (run)
                    second-run (run)]
                (is (= calls (count first-run)))
                (is (= first-run second-run))))]
    (is (:passed? result) (pr-str result))
    (is (<= 30 (get-in result [:statistics :valid])) (pr-str result))))

(defn- sequence-depth
  [value]
  (if (sequential? value)
    (inc (reduce max 0 (map sequence-depth value)))
    0))

(deftest recursion-limit-test
  (let [result
        (with {:test-cases 100 :seed 65 :database nil}
              [limit (g/integer 0 5)]
              (s/def ::limited-tree (s/or :leaf integer? :branch (s/tuple ::limited-tree)))
              (s/def ::limited-child (s/keys :opt [::limited-child]))
              (s/def ::limited-regex (s/alt :leaf integer?
                                            :branch (s/cat :node string? :child ::limited-regex)))
              (let [tree-generator (binding [sc/*recursion-limit* limit] (sc/gen ::limited-tree))
                    keys-generator (binding [sc/*recursion-limit* limit] (sc/gen ::limited-child))
                    regex-generator (binding [sc/*recursion-limit* limit] (sc/gen ::limited-regex))
                    tree (h/draw! tree-generator)
                    m (h/draw! keys-generator)
                    xs (h/draw! regex-generator)]
                (is (s/valid? ::limited-tree tree))
                (is (<= (sequence-depth tree) (max 1 limit)))
                (is (s/valid? ::limited-child m))
                (is (<= (count (take-while some? (iterate ::limited-child m)))
                        (inc (max 1 limit))))
                (is (s/valid? ::limited-regex xs))
                (is (<= (count xs) (inc (max 1 limit))))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest regex-repetition-length-test
  (let [result
        (with {:test-cases 100 :seed 66 :database nil}
              [minimum (g/integer 6 20)
               op (g/sampled-from [`s/* `s/+])]
              (let [spec (eval `(s/& (~op integer?) (fn [xs#] (<= ~minimum (count xs#)))))
                    generator (binding [sc/*recursion-limit* 0] (sc/gen spec))
                    value (h/draw! generator)]
                (is (s/valid? spec value))
                (is (<= minimum (count value)))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))
