(ns specel.override-test
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sg]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as tcg]
            [hegel-clj.clojure-test :refer [with]]
            [hegel-clj.core :as h]
            [hegel-clj.generator :as g]
            [specel.core :as sc]))

(defn- eval-spec
  [form]
  (binding [*ns* (the-ns 'specel.override-test)]
    (eval `(s/spec ~form))))

(defn- spec-reference
  [ident]
  (if (symbol? ident) `(quote ~ident) ident))

(defn- define-alias-chain!
  [prefix form depth symbols?]
  (let [target (keyword "specel.override-test" (str prefix "-target"))
        aliases (mapv (fn [i]
                        ((if (and symbols? (even? i)) symbol keyword)
                         "specel.override-test" (str prefix "-alias-" i)))
                      (range depth))]
    (eval `(s/def ~target ~form))
    (doseq [[alias previous] (map vector aliases (cons target aliases))]
      (eval `(s/def ~alias ~(spec-reference previous))))
    [target (peek aliases)]))

(defn- compare-overrides
  [spec entries native-entries expected]
  (let [overrides (fn [just entries]
                    (into {} (map (fn [[k v]] [k #(just v)])) entries))
        native (tcg/generate (s/gen spec (overrides sg/return native-entries)) 5 73)
        value (h/draw! (sc/gen spec (overrides g/just entries)))]
    (is (s/valid? spec native) (pr-str {:spec (s/form spec) :native native}))
    (is (s/valid? spec value) (pr-str {:spec (s/form spec) :value value}))
    (is (= expected native value)
        (pr-str {:spec (s/form spec) :entries entries
                 :expected expected :native native :value value}))))

(deftest nil-override-factories-test
  (doseq [regex? [false true]
          named? [false true]]
    (let [result
          (with {:test-cases 100 :seed 81 :database nil}
                [value (g/integer -1000 1000)]
                (let [target (if regex? ::nil-regex-target ::nil-regular-target)
                      form (if regex? `(s/cat :value #{~value}) `#{~value})
                      _ (eval `(s/def ~target ~form))
                      spec (eval-spec `(s/tuple ~target))
                      expected [(if regex? [value] value)]
                      overrides (cond-> {[0] (constantly nil)}
                                  named? (assoc target (constantly nil)
                                                [0] (fn []
                                                      (throw (ex-info "Selected name must not try path"
                                                                      {:target target})))))]
                  (if (and regex? named?)
                    (do
                      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Selected name"
                                            (s/gen spec overrides)))
                      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Selected name"
                                            (sc/gen spec overrides))))
                    (let [native (tcg/generate (s/gen spec overrides) 5 79)
                          generated (h/draw! (sc/gen spec overrides))]
                      (is (s/valid? spec generated))
                      (is (= expected native generated)
                          (pr-str {:regex? regex? :named? named?
                                   :expected expected :native native :value generated}))))))]
      (is (:passed? result) (pr-str result))
      (is (<= 100 (get-in result [:statistics :valid])) (pr-str result)))))

(deftest named-regular-alias-overrides-test
  (let [result
        (with {:test-cases 100 :seed 82 :database nil}
              [value (g/integer -1000 1000)
               depth (g/integer 1 4)]
              (doseq [symbols? [false true]]
                (let [[target alias] (define-alias-chain! "regular" `integer? depth symbols?)]
                  (doseq [nested? [false true]]
                    (let [reference (spec-reference alias)
                          spec (eval-spec (if nested? `(s/tuple ~reference) reference))]
                      (compare-overrides spec {target value} {target value}
                                         (if nested? [value] value)))))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest direct-alias-overrides-test
  (s/def ::direct-target integer?)
  (s/def ::direct-middle ::direct-target)
  (s/def ::direct-alias ::direct-middle)
  (let [result
        (with {:test-cases 1 :database nil} [n (g/just 42)]
              (doseq [ident [::direct-alias ::direct-middle ::direct-target]
                      [spec expected] [[::direct-alias n]
                                       [(s/tuple ::direct-alias) [n]]
                                       [(s/keys :req [::direct-alias]) {::direct-alias n}]
                                       [(s/keys :req-un [::direct-alias]) {:direct-alias n}]
                                       [(s/tuple (s/keys :req [::direct-alias])) [{::direct-alias n}]]]]
                (is (= expected (h/draw! (sc/gen spec {ident #(g/just n)})))
                    (pr-str {:ident ident :spec (s/form spec)})))
              (is (= {::direct-alias 1}
                     (h/draw! (sc/gen (s/keys :req [::direct-alias])
                                      {::direct-target #(g/just 1)
                                       ::direct-alias #(g/just 2)
                                       [::direct-alias] #(g/just 3)}))))
              (is (= {::direct-alias 2}
                     (h/draw! (sc/gen (s/keys :req [::direct-alias])
                                      {::direct-alias #(g/just 2)
                                       [::direct-alias] #(g/just 3)})))))]
    (is (:passed? result) (pr-str result))))

(deftest named-regex-alias-splicing-test
  (let [result
        (with {:test-cases 100 :seed 83 :database nil}
              [left (g/integer -1000 1000)
               right (g/integer -1000 1000)
               depth (g/integer 1 4)]
              (doseq [symbols? [false true]]
                (let [[target alias] (define-alias-chain!
                                       "regex" `(s/cat :left integer? :right integer?)
                                       depth symbols?)
                      reference (spec-reference alias)
                      pair [left right]]
                  (doseq [form [`(s/cat :pair ~reference :tail #{:tail})
                                `(s/& (s/+ ~reference)
                                      (fn [values#] (= 1 (count values#))))
                                `(s/& (s/* ~reference)
                                      (fn [values#] (= 1 (count values#))))]]
                    (compare-overrides (eval-spec form) {target pair} {target pair}
                                       (if (= `s/cat (first form))
                                         [left right :tail]
                                         pair))))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest parity-predicate-overrides-test
  (let [result
        (with {:test-cases 100 :seed 84 :database nil}
              [n (g/integer -1000 1000)]
              (doseq [predicate [`int? `integer?]
                      [parity value] [[`even? (* 2 n)] [`odd? (inc (* 2 n))]]]
                (compare-overrides (eval-spec `(s/and ~predicate ~parity))
                                   {predicate value} {[] value} value)))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest numeric-range-predicate-overrides-test
  (let [result
        (with {:test-cases 100 :seed 85 :database nil}
              [start (g/integer -10000 10000)
               width (g/integer 2 100)
               offset (g/integer 0 1)]
              (let [end (+ start width)
                    value (+ start offset)]
                (doseq [[form predicate selected]
                        [[`(s/int-in ~start ~end) `int? value]
                         [`(s/and integer?
                                  (fn [value#] (s/int-in-range? ~start ~end value#)))
                          `integer? value]
                         [`(s/double-in :min ~(double start) :max ~(double end)
                                        :NaN? false :infinite? false)
                          `double? (double value)]]]
                  (compare-overrides (eval-spec form) {predicate selected} {[] selected} selected))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest nil-range-override-fallback-test
  (let [result
        (with {:test-cases 100 :seed 87 :database nil}
              [start (g/integer 1000000000 1000000100)
               key (g/sampled-from [int? `int? []])]
              (let [spec (eval-spec `(s/int-in ~start ~(inc start)))
                    overrides {key (constantly nil)}
                    native (sg/generate (s/gen spec overrides))
                    value (h/draw! (sc/gen spec overrides))]
                (is (s/valid? spec value))
                (is (= start native value))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest alias-registry-extension-test
  (let [result
        (with {:test-cases 100 :seed 86 :database nil}
              [value (g/integer -1000 1000)]
              (s/def ::registry-target integer?)
              (s/def ::registry-alias ::registry-target)
              (is (= ::registry-alias (sc/def ::registry-alias (g/just value))))
              (doseq [form [::registry-alias `(s/tuple ::registry-alias)]]
                (let [spec (if (keyword? form) (eval `(identity ~form)) (eval-spec form))
                      generated (h/draw! (sc/gen spec))]
                  (is (s/valid? spec generated))
                  (is (= (if (keyword? form) value [value]) generated)))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))
