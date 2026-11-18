(ns specel.runtime-test
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sg]
            [clojure.test :refer [deftest is]]
            [hegel-clj.clojure-test :refer [with]]
            [hegel-clj.core :as h]
            [hegel-clj.generator :as g]
            [specel.core :as sc])
  (:import (clojure.lang MultiFn)
           (dev.hegel Generator)))

(s/def ::kind #{:a :b})
(s/def ::payload integer?)

(defmulti runtime-event :kind)
(defmethod runtime-event :a [_]
  (s/keys :req-un [::payload]))
(defmethod runtime-event :b [_]
  (s/keys :req-un [::payload]))

(deftest lexical-retag-test
  (let [result
        (with {:test-cases 100 :seed 81 :database nil}
              [tag (g/sampled-from [:a :b])
               payload (g/integer)
               stamp (g/string)]
              (let [retag (fn [value dispatch]
                            (assoc value :kind dispatch ::stamp stamp))
                    spec (s/multi-spec runtime-event retag)
                    value (h/draw! (sc/gen spec
                                           {[::sc/dispatch] #(g/just tag)
                                            [tag :payload] #(g/just payload)}))]
                (is (s/valid? spec value))
                (is (= {:kind tag :payload payload ::stamp stamp} value))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest lexical-multimethod-test
  (let [result
        (with {:test-cases 100 :seed 82 :database nil}
              [tag (g/sampled-from [:a :b])
               payload (g/integer)
               tag-key (g/sampled-from [:kind :type])
               stamp (g/integer)
               var? (g/boolean)]
              (let [mm (MultiFn. "lexical-event" tag-key :default #'clojure.core/global-hierarchy)
                    retag (fn [value dispatch]
                            (assoc value tag-key dispatch ::stamp stamp))]
                (doseq [dispatch [:a :b]]
                  (.addMethod mm dispatch
                              (fn [_]
                                (s/and (s/keys :req-un [::payload])
                                       #(= dispatch (get % tag-key))
                                       #(= stamp (::stamp %))))))
                (with-local-vars [local-mm mm]
                  (let [source (if var? local-mm (atom mm))
                        spec (s/multi-spec-impl 'lost.lexical/multimethod source retag)
                        value (h/draw! (sc/gen spec
                                               {[::sc/dispatch] #(g/just tag)
                                                [tag :payload] #(g/just payload)}))]
                    (is (s/valid? spec value))
                    (is (= {tag-key tag :payload payload ::stamp stamp} value))))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest runtime-multimethod-factory-test
  (let [result
        (with {:test-cases 50 :seed 83 :database nil}
              [tag (g/sampled-from [:a :b])]
              (let [factory (get-in (sc/registry) [:forms `s/multi-spec])
                    context {:registry (assoc (sc/registry) ::sc/recursion-limit 4)
                             :overrides {[::sc/dispatch] #(g/just tag)
                                         ::payload #(g/integer)}
                             :path [] :ancestors {}}]
                (doseq [source [runtime-event #'runtime-event]]
                  (let [generator (factory (list `s/multi-spec source :kind) context)
                        value (h/draw! generator)]
                    (is (instance? Generator generator))
                    (is (= tag (:kind value)))
                    (is (integer? (:payload value)))))))]
    (is (:passed? result) (pr-str result))))

(defn- eval-runtime-form
  [form]
  (binding [*ns* (the-ns 'specel.runtime-test)]
    (eval form)))

(defn- attached-generator-forms
  [form]
  [form
   `(s/with-gen ~form nil)
   `(s/with-gen ~form
      (fn [] (throw (ex-info "Must not invoke attached generator" {}))))])

(deftest expression-integer-bounds-test
  (let [result
        (with {:test-cases 50 :seed 101 :database nil}
              [n (g/integer 0 1000000)]
              (doseq [form (attached-generator-forms
                            `(let [~'n ~n] (s/int-in 0 (inc ~'n))))]
                (let [spec (eval-runtime-form form)
                      value (h/draw! (sc/gen spec))]
                  (is (s/valid? spec value) (pr-str form))
                  (is (<= 0 value n)))))]
    (is (:passed? result) (pr-str result))
    (is (<= 50 (get-in result [:statistics :valid])) (pr-str result))))

(deftest narrow-lexical-integer-bounds-test
  (let [result
        (with {:test-cases 50 :seed 102 :database nil}
              [lower-bound (g/long {:min 1000000000000 :max 1000000000100})
               width (g/integer 1 4)]
              (doseq [range-form [`(s/int-in ~'lower-bound (+ ~'lower-bound ~'width))
                                  `(s/and int?
                                          (fn [value#]
                                            (s/int-in-range? ~'lower-bound ~'upper-bound value#)))]
                      form (attached-generator-forms
                            `(let [~'lower-bound ~lower-bound
                                   ~'width ~width
                                   ~'upper-bound (+ ~'lower-bound ~'width)]
                               ~range-form))]
                (let [spec (eval-runtime-form form)
                      value (h/draw! (sc/gen spec))]
                  (is (s/valid? spec value) (pr-str form))
                  (is (<= lower-bound value (dec (+ lower-bound width)))))))]
    (is (:passed? result) (pr-str result))
    (is (<= 50 (get-in result [:statistics :valid])) (pr-str result))))

(deftest unreachable-range-bounds-test
  (let [result
        (with {:test-cases 50 :seed 108 :database nil}
              [expected (g/integer)]
              (let [spec (s/and int?
                                (fn [x]
                                  (if false (s/int-in-range? (/ 1 0) 10 x) true)))
                    generator (sc/gen spec)
                    value (h/draw! generator)
                    selected (h/draw! (sc/gen spec {int? #(g/just expected)}))]
                (is (instance? Generator generator))
                (is (s/valid? spec value))
                (is (s/valid? spec selected))
                (is (= expected selected))))]
    (is (:passed? result) (pr-str result))
    (is (<= 50 (get-in result [:statistics :valid])) (pr-str result))))

(deftest failed-bound-arithmetic-falls-back-test
  (let [spec (s/and int?
                    (fn [x]
                      (s/int-in-range? (/ 1 0) 10 x)))]
    (is (instance? Generator (sc/gen spec)))))

(deftest expression-double-bounds-test
  (let [result
        (with {:test-cases 50 :seed 103 :database nil}
              [lower-bound (g/integer 1000000000 1000000100)]
              (doseq [form (attached-generator-forms
                            `(let [~'lower-bound ~lower-bound]
                               (s/double-in :min (double ~'lower-bound)
                                            :max (+ (double ~'lower-bound) 0.00001)
                                            :NaN? false :infinite? false)))]
                (let [spec (eval-runtime-form form)
                      value (h/draw! (sc/gen spec))]
                  (is (s/valid? spec value) (pr-str form))
                  (is (double? value))
                  (is (<= (double lower-bound) value (+ (double lower-bound) 0.00001))))))]
    (is (:passed? result) (pr-str result))
    (is (<= 50 (get-in result [:statistics :valid])) (pr-str result))))

(deftest lexical-instant-bounds-test
  (let [result
        (with {:test-cases 50 :seed 104 :database nil}
              [start (g/long {:min -1000000000000 :max 1000000000000})
               width (g/integer 1 4)]
              (doseq [form (attached-generator-forms
                            `(let [~'lower-bound (java.util.Date. ~start)
                                   ~'upper-bound (java.util.Date. ~(+ start width))]
                               (s/inst-in ~'lower-bound ~'upper-bound)))]
                (let [spec (eval-runtime-form form)
                      value (h/draw! (sc/gen spec))]
                  (is (s/valid? spec value) (pr-str form))
                  (is (instance? java.util.Date value))
                  (is (<= start (inst-ms value) (dec (+ start width)))))))]
    (is (:passed? result) (pr-str result))
    (is (<= 50 (get-in result [:statistics :valid])) (pr-str result))))

(deftest opaque-integer-bounds-test
  (let [result
        (with {:test-cases 40 :seed 105 :database nil}
              [expected (g/integer -1000 1000)]
              (doseq [bound-form [`(identity ~'lower-bound)
                                  `(~'read-bound)
                                  `(+ ~'lower-bound (identity 0))
                                  `(+ ~'lower-bound (~'read-zero))]
                      range-form [`(s/int-in ~bound-form ~'upper-bound)
                                  `(s/and int?
                                          (fn [value#]
                                            (s/int-in-range? ~bound-form ~'upper-bound value#)))]]
                (let [[spec calls]
                      (eval-runtime-form
                       `(let [~'lower-bound Long/MIN_VALUE
                              ~'upper-bound Long/MAX_VALUE
                              ~'calls (atom 0)
                              ~'read-bound (fn [] (swap! ~'calls inc) ~'lower-bound)
                              ~'read-zero (fn [] (swap! ~'calls inc) 0)]
                          [(s/with-gen ~range-form nil) ~'calls]))
                      generator (sc/gen spec)
                      overridden (sc/gen spec {int? #(g/just expected)})]
                  (is (instance? Generator generator) (pr-str range-form))
                  (is (zero? @calls) (pr-str range-form))
                  (let [value (h/draw! generator)
                        selected (h/draw! overridden)]
                    (is (s/valid? spec value) (pr-str range-form))
                    (is (s/valid? spec selected) (pr-str range-form))
                    (is (= expected selected))
                    (when (#{`(~'read-bound) `(+ ~'lower-bound (~'read-zero))} bound-form)
                      (is (pos? @calls)))))))]
    (is (:passed? result) (pr-str result))
    (is (<= 40 (get-in result [:statistics :valid])) (pr-str result))))

(deftest computed-predicate-wrappers-test
  (let [result
        (with {:test-cases 50 :seed 106 :database nil}
              [n (g/integer -1000 1000)]
              (doseq [form [`(s/spec (identity int?))
                            `(let [~'p int?] (s/spec (do ~'p)))]]
                (let [spec (eval-runtime-form form)
                      value (h/draw! (sc/gen spec))
                      constrained (s/and spec even?)
                      selected (h/draw! (sc/gen constrained
                                                {int? #(g/sampled-from [(* 2 n) (inc (* 2 n))])}))]
                  (is (int? value) (pr-str form))
                  (is (s/valid? spec value) (pr-str form))
                  (is (= (* 2 n) selected) (pr-str form))
                  (is (s/valid? constrained selected) (pr-str form)))))]
    (is (:passed? result) (pr-str result))
    (is (<= 50 (get-in result [:statistics :valid])) (pr-str result))))

(deftest computed-set-finite-domain-sizing-test
  (let [result
        (with {:test-cases 40 :seed 107 :database nil}
              [domain (g/set {:min-size 1 :max-size 5} (g/integer -10 10))
               extra (g/integer 1 3)
               size (g/integer 0 5)]
              (let [capacity (count domain)
                    feasible (mod size (inc capacity))]
                (doseq [element [`(s/spec (into #{} ~(vec domain)))
                                 `(let [domain# (into #{} ~(vec domain))] (s/spec domain#))]
                        op [`s/coll-of `s/map-of]
                        option [:count :min-count]
                        feasible [0 feasible capacity]]
                  (let [collection-form
                        (fn [required]
                          (if (= `s/coll-of op)
                            `(s/coll-of ~element :distinct true ~option ~required :max-count ~required)
                            `(s/map-of ~element int? ~option ~required :max-count ~required)))
                        impossible (eval-runtime-form (collection-form (+ capacity extra)))
                        error (try (sc/gen impossible) nil
                                   (catch clojure.lang.ExceptionInfo e e))
                        spec (eval-runtime-form (collection-form feasible))
                        value (h/draw! (sc/gen spec))]
                    (is (some? error) (pr-str (s/form impossible)))
                    (is (= capacity (:capacity (ex-data error))))
                    (is (= (+ capacity extra) (:min-count (ex-data error))))
                    (is (s/valid? spec value) (pr-str (s/form spec)))
                    (is (= feasible (count value)))
                    (is (every? #(contains? domain %) (if (map? value) (keys value) value)))))))]
    (is (:passed? result) (pr-str result))
    (is (<= 40 (get-in result [:statistics :valid])) (pr-str result))))

(deftest multi-spec-qualified-tag-override-test
  (let [result
        (with {:test-cases 50 :seed 109 :database nil}
              [selected (g/sampled-from [:a :b])
               payload (g/integer)]
              (doseq [form [`(s/keys :req-un [(and ::kind ::payload)])
                            `(s/and (identity (s/keys :req-un [(and ::kind ::payload)])))
                            `(s/merge (identity (s/keys :req-un [(and ::kind ::payload)]))
                                      (s/keys))]]
                (let [mm (MultiFn. "qualified-tag-event" :kind :default #'clojure.core/global-hierarchy)]
                  (doseq [tag [:a :b]]
                    (.addMethod mm tag (fn [_] (eval-runtime-form form))))
                  (with-local-vars [source mm]
                    (let [spec (s/multi-spec-impl 'qualified-tag-event source :kind)
                          value (h/draw! (sc/gen spec
                                                 {::kind #(g/just selected)
                                                  ::payload #(g/just payload)}))]
                      (is (= {:kind selected :payload payload} value) (pr-str form))
                      (is (s/valid? spec value) (pr-str form)))))))]
    (is (:passed? result) (pr-str result))
    (is (<= 50 (get-in result [:statistics :valid])) (pr-str result))))

(deftest multi-spec-default-dispatch-test
  (let [result
        (with {:test-cases 100 :seed 94 :database nil}
              [default-dispatch (g/one-of (mapv g/just [:default ::fallback nil false 42]))
               payload (g/integer)]
              (let [tag (if (= :default default-dispatch) :a :default)
                    mm (MultiFn. "default-event" :kind default-dispatch #'clojure.core/global-hierarchy)]
                (.addMethod mm tag (fn [_] (s/keys :req-un [::payload])))
                (.addMethod mm default-dispatch
                            (fn [_] (throw (ex-info "Default method must not be generated" {}))))
                (with-local-vars [source mm]
                  (let [spec (s/multi-spec-impl 'default-event source :kind)]
                    (doseq [overrides [{::payload #(g/just payload)}
                                       {::payload #(g/just payload)
                                        [::sc/dispatch] #(g/just tag)}]]
                      (let [value (h/draw! (sc/gen spec overrides))]
                        (is (= {:kind tag :payload payload} value))
                        (is (s/valid? spec value))))))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest multi-spec-default-selector-test
  (doseq [default-dispatch [:default ::fallback nil false 42]]
    (let [tag (if (= :default default-dispatch) :a :default)
          mm (MultiFn. "default-selector" :kind default-dispatch #'clojure.core/global-hierarchy)]
      (.addMethod mm tag (fn [_] (s/keys)))
      (.addMethod mm default-dispatch (fn [_] (s/keys)))
      (with-local-vars [source mm]
        (let [spec (s/multi-spec-impl 'default-selector source :kind)
              generator (sc/gen spec {[::sc/dispatch] #(g/just default-dispatch)})
              result (h/test-fn! {:test-cases 10 :seed 1 :database nil :phases [:generate]}
                                 (fn [_] (h/draw! generator)))]
          (is (empty? (:failures result)) (pr-str result))
          (is (zero? (get-in result [:statistics :valid] 0)) (pr-str result)))
        (let [spec (s/multi-spec-impl 'default-selector source :kind)
              generator (sc/gen spec {[::sc/dispatch] #(g/just ::unknown)})
              result (h/test-fn! {:test-cases 1 :seed 1 :database nil :phases [:generate]}
                                 (fn [_]
                                   (let [value (h/draw! generator)]
                                     (is (= ::unknown (:kind value)))
                                     (is (s/valid? spec value)))))]
          (is (:passed? result) (pr-str result))
          (is (pos? (get-in result [:statistics :valid] 0)) (pr-str result)))))))

(deftest runtime-set-specs-test
  (let [result
        (with {:test-cases 100 :seed 95 :database nil}
              [numbers (g/set {:min-size 1 :max-size 6} (g/integer -20 20))]
              (let [domain (into #{nil false} numbers)
                    values (vec domain)
                    forms [`(s/spec ~domain)
                           `(s/spec (into #{} ~values))
                           `(let [domain# (into #{} ~values)] (s/spec domain#))
                           `(do (s/def ::computed-set (into #{} ~values)) ::computed-set)
                           `(do (def runtime-domain (into #{} ~values))
                                (s/def ::computed-set runtime-domain)
                                ::computed-set)]]
                (doseq [form forms]
                  (let [spec (eval-runtime-form form)
                        value (h/draw! (sc/gen spec))]
                    (is (contains? domain value) (pr-str form))
                    (is (s/valid? spec value) (pr-str form))))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(defn- recursive-multi-spec
  [branch-form]
  (eval-runtime-form
   `(do
      (defmulti productive-tree :kind)
      (defmethod productive-tree :leaf [~'_]
        (s/keys :req-un [::payload]))
      (defmethod productive-tree :branch [~'_]
        ~branch-form)
      (s/def ::tree-child (s/multi-spec productive-tree :kind))
      (s/get-spec ::tree-child))))

(deftest productive-multi-spec-recursion-test
  (let [result
        (with {:test-cases 100 :seed 1 :database nil :phases [:generate]}
              [form (g/sampled-from
                     [`(s/keys :req-un [::tree-child])
                      `(s/keys :req [::tree-child])
                      `(s/merge (s/keys :req-un [::tree-child])
                                (s/keys :req-un [::payload]))
                      `(s/and (s/keys :req-un [::tree-child])
                              (fn [~'value] (= :branch (:kind ~'value))))])
               limit (g/integer 0 4)]
              (let [spec (recursive-multi-spec form)
                    generator (binding [sc/*recursion-limit* limit] (sc/gen spec))
                    value (h/draw! generator)]
                (is (s/valid? spec value) (pr-str {:form form :limit limit :value value}))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest fresh-method-multi-spec-recursion-test
  (let [result
        (with {:test-cases 100 :seed 88 :database nil :phases [:generate]}
              [limit (g/integer 0 4)]
              (let [spec (recursive-multi-spec
                          `(s/merge (s/keys :req-un [::payload])
                                    (s/multi-spec productive-tree :kind)))
                    generator (binding [sc/*recursion-limit* limit]
                                (sc/gen spec {[::sc/dispatch] #(g/just :leaf)}))
                    value (h/draw! generator)]
                (is (= :leaf (:kind value)))
                (is (s/valid? spec value))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest mutual-multi-spec-recursion-test
  (let [result
        (with {:test-cases 100 :seed 89 :database nil :phases [:generate]}
              [limit (g/integer 0 4)
               qualified? (g/boolean)]
              (let [form (if qualified?
                           `(s/keys :req [::mutual-child])
                           `(s/keys :req-un [::mutual-child]))
                    spec (recursive-multi-spec form)]
                (eval-runtime-form
                 `(do
                    (defmulti mutual-tree :kind)
                    (defmethod mutual-tree :leaf [~'_]
                      (s/keys :req-un [::payload]))
                    (defmethod mutual-tree :branch [~'_]
                      (s/keys :req-un [::tree-child]))
                    (s/def ::mutual-child (s/multi-spec mutual-tree :kind))))
                (let [generator (binding [sc/*recursion-limit* limit] (sc/gen spec))
                      value (h/draw! generator)]
                  (is (s/valid? spec value) (pr-str {:limit limit :value value})))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest required-child-multi-spec-recursion-test
  (let [spec (recursive-multi-spec `(s/keys :req-un [::tree-child]))
        generator (sc/gen spec)
        result (h/test-fn! {:test-cases 100 :seed 1 :database nil :phases [:generate]}
                           (fn [_]
                             (is (s/valid? spec (h/draw! generator)))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest required-multi-spec-cycle-errors-test
  (let [result
        (with {:test-cases 100 :seed 90 :database nil :phases [:generate]}
              [limit (g/integer 0 4)
               forced? (g/boolean)]
              (let [spec (recursive-multi-spec `(s/keys :req-un [::tree-child]))]
                (eval-runtime-form `(remove-method productive-tree :leaf))
                (let [error (try
                              (binding [sc/*recursion-limit* limit]
                                (sc/gen spec (when forced?
                                               {[::sc/dispatch] #(g/just :branch)})))
                              nil
                              (catch clojure.lang.ExceptionInfo e e))]
                  (is (::sc/no-finite-generator (ex-data error))))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest impossible-multi-spec-selector-test
  (let [spec (recursive-multi-spec `(s/keys :req-un [::tree-child]))
        generator (binding [sc/*recursion-limit* 0]
                    (sc/gen spec {[::sc/dispatch] #(g/just :branch)}))
        result (h/test-fn! {:test-cases 10 :seed 1 :database nil :phases [:generate]}
                           (fn [_] (h/draw! generator)))]
    (is (empty? (:failures result)) (pr-str result))
    (is (zero? (get-in result [:statistics :valid] 0)) (pr-str result))))

(deftest multi-spec-branches-preserve-errors-test
  (let [spec (recursive-multi-spec `(s/conformer identity))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Conformer requires a generator override"
                          (sc/gen spec {[::sc/dispatch] #(g/just :leaf)})))))

(deftest trivial-function-relation-test
  (let [result
        (with {:test-cases 40 :seed 84 :database nil}
              [n (g/integer)]
              (let [spec (s/fspec :args (s/cat :n integer?)
                                  :ret integer?
                                  :fn (constantly true))
                    f (h/draw! (sc/gen spec))]
                (is (s/valid? spec f))
                (is (integer? (f n)))
                (is (integer? (binding [h/*test-case* nil] (f n))))))]
    (is (:passed? result) (pr-str result))
    (is (<= 40 (get-in result [:statistics :valid])) (pr-str result))))

(deftest conformed-function-relation-test
  (let [result
        (with {:test-cases 60 :seed 85 :database nil}
              [n (g/integer -10 10)
               outside? (g/boolean)]
              (let [input (s/conformer inc dec)
                    output (s/conformer #(* 2 %) #(/ % 2))
                    spec (s/fspec :args (s/cat :n (s/and (s/int-in -10 11) input))
                                  :ret (s/and (s/int-in -20 21) output)
                                  :fn (fn [{:keys [args ret]}]
                                        (= (even? (:n args)) (pos? ret))))
                    f (h/draw! (sc/gen spec))
                    value (if outside?
                            (binding [h/*test-case* nil] (f n))
                            (f n))]
                (is (s/valid? spec f))
                (is (s/valid? (:ret spec) value))
                (is (= (even? (inc n)) (pos? (* 2 value))))))]
    (is (:passed? result) (pr-str result))
    (is (<= 60 (get-in result [:statistics :valid])) (pr-str result))))

(deftest function-return-root-context-test
  (let [result
        (with {:test-cases 40 :seed 86 :database nil}
              [expected (g/integer)
               input (g/integer)]
              (let [function-spec (s/fspec :args (s/cat :n integer?)
                                           :ret (s/tuple integer?))
                    spec (s/or :callable function-spec)
                    native (sg/generate (s/gen spec {[0] #(sg/return expected)}))
                    f (h/draw! (sc/gen spec
                                       {[0] #(g/just expected)
                                        [:callable :ret 0]
                                        #(throw (ex-info "Nested return path used" {}))}))]
                (is (= [expected] (native input)))
                (is (= [expected] (f input)))))]
    (is (:passed? result) (pr-str result))))

(deftest relational-function-fresh-draws-test
  (let [result
        (with {:test-cases 40 :seed 87 :database nil}
              [start (g/integer -100 100)
               calls (g/integer 2 8)
               outside? (g/boolean)]
              (let [draws (atom start)
                    fail? (atom false)
                    return-generator (reify Generator
                                       (doDraw [_ _]
                                         (if @fail?
                                           (throw (ex-info "Return generation failed" {:start start}))
                                           (swap! draws inc))))
                    spec (s/fspec :args (s/cat :n integer?)
                                  :ret integer?
                                  :fn (constantly true))
                    f (h/draw! (sc/gen spec {integer? #(identity return-generator)}))]
                (reset! draws start)
                (let [values (if outside?
                               (binding [h/*test-case* nil] (mapv f (range calls)))
                               (mapv f (range calls)))]
                  (is (= (vec (range (inc start) (+ start calls 1))) values)))
                (is (thrown? clojure.lang.ExceptionInfo (f :invalid)))
                (is (= (+ start calls) @draws))
                (reset! fail? true)
                (let [exception (try
                                  (binding [h/*test-case* nil] (f start))
                                  nil
                                  (catch clojure.lang.ExceptionInfo e e))]
                  (is (= "Return generation failed" (ex-message exception)))
                  (is (= {:start start} (ex-data exception))))))]
    (is (:passed? result) (pr-str result))
    (is (<= 40 (get-in result [:statistics :valid])) (pr-str result))))
