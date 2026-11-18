(ns specel.factory-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is]]
            [hegel-clj.clojure-test :refer [with]]
            [hegel-clj.core :as h]
            [hegel-clj.generator :as g]
            [specel.core :as sc]))

(defn- eval-form
  [form]
  (binding [*ns* (the-ns 'specel.factory-test)]
    (try
      (eval form)
      (catch clojure.lang.Compiler$CompilerException e
        (throw (ex-cause e))))))

(defn- eval-spec
  [form]
  (eval-form `(s/spec ~form)))

(defn- check-report
  [report]
  (is (:passed? report) (pr-str report))
  (is (pos? (get-in report [:statistics :valid] 0)) (pr-str report)))

(defn- nested-form
  [form depth]
  (nth (iterate #(list `s/tuple %) form) depth))

(defn- nested-value
  [value depth]
  (nth (iterate vector value) depth))

(deftest factory-path-and-descendant-overrides-test
  (check-report
   (with {:test-cases 50 :seed 101 :database nil}
         [marker (g/integer -1000 1000)
          value (g/integer -1000 1000)
          depth (g/integer 0 3)
          keyed? (g/boolean)]
         (eval-form `(s/def ::assembled (s/tuple #{~marker} integer?)))
         (let [calls (atom [])
               override-calls (atom 0)
               paths (if keyed? [[:assembled]] [[0] [1]])
               paths (mapv #(into (vec (repeat depth 0)) %) paths)
               overrides (into {} (map (fn [path]
                                         [(conj path 1)
                                          (fn []
                                            (swap! override-calls inc)
                                            (g/just value))]))
                               paths)
               factory (fn [overrides path rmap]
                         (swap! calls conj [path rmap])
                         (g/fmap #(vector marker %)
                                 ((get overrides (conj path 1)))))
               form (nested-form (if keyed?
                                   `(s/keys :req-un [::assembled])
                                   `(s/tuple ::assembled ::assembled))
                                 depth)
               spec (eval-spec form)
               expected (nested-value (if keyed?
                                        {:assembled [marker value]}
                                        [[marker value] [marker value]])
                                      depth)]
           (is (= ::assembled (sc/def* ::assembled factory)))
           (is (identical? factory (get-in (sc/registry) [:idents ::assembled])))
           (let [generated (h/draw! (sc/gen spec overrides))]
             (is (s/valid? spec generated) (pr-str {:form form :value generated}))
             (is (= expected generated))
             (is (= (set paths) (set (map first @calls))))
             (is (= (count paths) (count @calls) @override-calls))
             (is (every? #(contains? (second %) ::s/recursion-limit) @calls)))))))

(deftest factory-default-recursion-limit-capture-test
  (check-report
   (with {:test-cases 50 :seed 102 :database nil}
         [value (g/integer -1000 1000)
          depth (g/integer 0 3)
          delta (g/integer 1 10)]
         (eval-form `(s/def ::captured #{~value}))
         (let [calls (atom [])
               default-limit sc/*recursion-limit*
               _ (sc/def* ::captured
                   (fn [_ path rmap]
                     (swap! calls conj [path (::s/recursion-limit rmap)])
                     (g/just value)))
               spec (eval-spec (nested-form `(s/fspec :args (s/cat) :ret ::captured) depth))
               generator (sc/gen spec)]
           (is (empty? @calls))
           (binding [sc/*recursion-limit* (+ default-limit delta)]
             (let [generated (h/draw! generator)
                   f (nth (iterate first generated) depth)]
               (is (= value (f)))
               (is (s/valid? spec generated))))
           (is (seq @calls))
           (is (every? #(= [[] default-limit] %) @calls))))))

(deftest factory-alias-registration-test
  (check-report
   (with {:test-cases 50 :seed 103 :database nil :phases [:generate]}
         [value (g/integer -1000 1000)
          depth (g/integer 0 3)
          alias-depth (g/integer 1 4)]
         (eval-form `(s/def ::alias-target (s/int-in ~value ~(+ value 10))))
         (let [target (reduce (fn [previous i]
                                (let [ident (keyword "specel.factory-test" (str "link-" i))]
                                  (eval-form `(s/def ~ident ~previous))
                                  ident))
                              ::alias-target (range alias-depth))
               calls (atom [])
               factory (fn [_ path _]
                         (swap! calls conj path)
                         (g/just value))]
           (eval-form `(s/def ::keyword-alias ~target))
           (eval-form `(s/def specel.factory-test/symbol-alias ~target))
           (is (= ::keyword-alias (sc/def* ::keyword-alias factory)))
           (is (= 'specel.factory-test/symbol-alias
                  (sc/def* specel.factory-test/symbol-alias factory)))
           (doseq [reference [::keyword-alias `(quote specel.factory-test/symbol-alias)]]
             (let [spec (if (zero? depth)
                          (eval-form reference)
                          (eval-spec (nested-form reference depth)))
                   before (count @calls)
                   generated (h/draw! (sc/gen spec))]
               (is (s/valid? spec generated))
               (is (= (nested-value value depth) generated))
               (is (= (inc before) (count @calls)))
               (is (= (vec (repeat depth 0)) (peek @calls)))))))))

(deftest factory-and-generator-replacement-test
  (check-report
   (with {:test-cases 50 :seed 104 :database nil}
         [value (g/integer -1000 1000)
          depth (g/integer 0 3)
          form (g/sampled-from [`integer? `(s/and integer? number?)
                                `(s/int-in -2000 2000)])]
         (eval-form `(s/def ::replacement ~form))
         (let [calls (atom 0)
               factory (fn [_ _ _]
                         (swap! calls inc)
                         (g/just value))
               spec (if (zero? depth)
                      (eval-form ::replacement)
                      (eval-spec (nested-form ::replacement depth)))]
           (sc/def ::replacement (g/just (inc value)))
           (sc/def* ::replacement factory)
           (is (= (nested-value value depth) (h/draw! (sc/gen spec))))
           (is (= 1 @calls))
           (let [generator (g/just (inc value))]
             (sc/def ::replacement generator)
             (is (identical? generator (get-in (sc/registry) [:idents ::replacement])))
             (let [generated (h/draw! (sc/gen spec))]
               (is (s/valid? spec generated))
               (is (= (nested-value (inc value) depth) generated))))
           (is (= 1 @calls))
           (sc/def* ::replacement factory)
           (let [generated (h/draw! (sc/gen spec))]
             (is (s/valid? spec generated))
             (is (= (nested-value value depth) generated)))
           (is (= 2 @calls))))))

(deftest nil-factory-default-derivation-test
  (check-report
   (with {:test-cases 50 :seed 105 :database nil}
         [value (g/integer -1000 1000)
          depth (g/integer 0 3)
          tuple? (g/boolean)]
         (eval-form `(s/def ::nil-target ~(if tuple? `(s/tuple #{~value}) `#{~value})))
         (eval-form `(s/def ::nil-alias ::nil-target))
         (let [calls (atom 0)
               target-draws (atom 0)
               expected (if tuple? [value] value)
               factory (fn [_ _ _] (swap! calls inc) nil)]
           (sc/def ::nil-target
             (g/fmap (fn [_] (swap! target-draws inc) expected) (g/just nil)))
           (sc/def* ::nil-alias factory)
           (let [spec (if (zero? depth)
                        (eval-form ::nil-alias)
                        (eval-spec (nested-form ::nil-alias depth)))
                 generated (h/draw! (sc/gen spec))]
             (is (s/valid? spec generated))
             (is (= (nested-value expected depth) generated))
             (is (= 1 @calls))
             (is (zero? @target-draws)))
           (sc/def* ::nil-target factory)
           (reset! calls 0)
           (let [spec (eval-spec (nested-form ::nil-target depth))
                 generated (h/draw! (sc/gen spec))]
             (is (s/valid? spec generated))
             (is (= (nested-value expected depth) generated))
             (is (= 1 @calls)))))))

(deftest invalid-factory-registration-and-result-test
  (check-report
   (with {:test-cases 50 :seed 106 :database nil}
         [ident (g/one-of [(g/just nil)
                           (g/sampled-from [:unqualified 'unqualified "qualified/name" 42])])
          invalid (g/one-of [(g/just nil) (g/sampled-from [false 42 "not a factory"])])
          result (g/sampled-from [false 42 :not-a-generator [1] {:value 1}])
          depth (g/integer 0 3)
          form (g/sampled-from [`integer? `(s/tuple integer?) `(s/nilable integer?)])]
         (is (thrown-with-msg? clojure.lang.ExceptionInfo #"namespace-qualified"
                               (eval-form `(sc/def* ~ident (fn [~'_ ~'_ ~'_] nil)))))
         (is (thrown-with-msg? clojure.lang.ExceptionInfo #"generator factory"
                               (sc/def* ::invalid-registration invalid)))
         (eval-form `(s/def ::invalid-result ~form))
         (sc/def* ::invalid-result (fn [_ _ _] result))
         (let [spec (if (zero? depth)
                      (eval-form ::invalid-result)
                      (eval-spec (nested-form ::invalid-result depth)))]
           (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Hegel generator or nil"
                                 (sc/gen spec)))))))

(deftest explicit-overrides-bypass-factory-test
  (check-report
   (with {:test-cases 50 :seed 107 :database nil}
         [value (g/integer -1000 1000)
          depth (g/integer 0 3)]
         (eval-form `(s/def ::bypass (s/int-in ~value ~(+ value 3))))
         (let [calls (atom 0)
               _ (sc/def* ::bypass (fn [_ _ _] (swap! calls inc) (g/just (+ value 2))))
               path (vec (repeat depth 0))
               spec (if (zero? depth)
                      (eval-form ::bypass)
                      (eval-spec (nested-form ::bypass depth)))]
           (doseq [overrides [{::bypass #(g/just value)}
                              {path #(g/just value)}
                              {::bypass #(g/just value)
                               path (fn [] (throw (ex-info "Name must precede path" {})))}]]
             (let [generated (h/draw! (sc/gen spec overrides))]
               (is (s/valid? spec generated))
               (is (= (nested-value value depth) generated))))
           (is (zero? @calls))
           (let [generated (h/draw! (sc/gen spec {path (constantly nil)}))]
             (is (s/valid? spec generated))
             (is (= (nested-value (+ value 2) depth) generated)))
           (is (= 1 @calls))))))

(deftest multi-spec-tag-factory-path-test
  (check-report
   (with {:test-cases 50 :seed 108 :database nil}
         [tag (g/sampled-from [:a :b])
          value (g/integer -1000 1000)
          depth (g/integer 0 3)
          payload-form (g/sampled-from [`integer? `(s/and integer? number?)])]
         (eval-form
          `(do
             (s/def ::kind #{:a :b})
             (s/def ::payload ~payload-form)
             (defmulti tag-event :kind)
             (defmethod tag-event :a [~'_] (s/keys :req-un [::kind ::payload]))
             (defmethod tag-event :b [~'_] (s/keys :req-un [::kind ::payload]))))
         (let [calls (atom [])
               _ (sc/def* ::kind
                   (fn [_ path rmap]
                     (swap! calls conj [path rmap])
                     (g/just tag)))
               spec (eval-spec (nested-form `(s/multi-spec tag-event :kind) depth))
               generated (h/draw! (sc/gen spec {::payload #(g/just value)}))]
           (is (s/valid? spec generated))
           (is (= (nested-value {:kind tag :payload value} depth) generated))
           (is (= [(conj (vec (repeat depth 0)) :kind)] (mapv first @calls)))
           (is (every? #(contains? (second %) ::s/recursion-limit) @calls))))))

(deftest factory-predicate-range-and-fallback-test
  (let [original (get-in (sc/registry) [:idents `int?])]
    (try
      (check-report
       (with {:test-cases 50 :seed 110 :database nil}
             [value (g/integer -1000 1000)
              depth (g/integer 0 3)]
             (let [calls (atom [])
                   spec (eval-spec (nested-form `(s/int-in ~value ~(inc value)) depth))]
               (sc/def* clojure.core/int?
                 (fn [_ path _]
                   (swap! calls conj path)
                   (g/just value)))
               (let [generated (h/draw! (sc/gen spec))]
                 (is (s/valid? spec generated))
                 (is (= (nested-value value depth) generated))
                 (is (= [(vec (repeat depth 0))] @calls)))
               (sc/def* clojure.core/int? (fn [_ _ _] nil))
               (doseq [form [`int? `(s/int-in ~value ~(inc value))]]
                 (let [spec (eval-spec form)
                       generated (h/draw! (sc/gen spec))]
                   (is (s/valid? spec generated)))))))
      (finally
        (sc/def clojure.core/int? original)))))

(deftest recursive-factory-path-local-counts-test
  (check-report
   (with {:test-cases 50 :seed 109 :database nil}
         [value (g/integer -1000 1000)
          cutoff (g/integer 1 3)
          nested? (g/boolean)]
         (eval-form
          `(s/def ::recursive
             (s/or :leaf #{~value}
                   :branch ~(if nested?
                              `(s/tuple (s/tuple ::recursive) ::recursive)
                              `(s/tuple ::recursive ::recursive)))))
         (let [calls (atom [])
               recursive-spec (s/get-spec ::recursive)
               _ (sc/def* ::recursive
                   (fn [_ path rmap]
                     (let [n (get rmap recursive-spec 0)]
                       (swap! calls conj [path n rmap])
                       (when (>= n cutoff) (g/just value)))))
               spec (eval-form ::recursive)
               generator (binding [sc/*recursion-limit* (+ cutoff 2)] (sc/gen spec))
               generated (h/draw! generator)]
           (is (s/valid? spec generated))
           (is (= (dec (bit-shift-left 1 (inc cutoff))) (count @calls)))
           (doseq [[path n rmap] @calls]
             (is (= (count (filter #{:branch} path)) n) (pr-str [path n]))
             (is (= (+ cutoff 2) (::s/recursion-limit rmap))))
           (doseq [n (range (inc cutoff))]
             (is (= (bit-shift-left 1 n) (count (filter #(= n (second %)) @calls)))))))))

(deftest contextual-generation-overrides-and-path-test
  (check-report (with {:test-cases 1 :seed 201 :database nil} []
                      (s/def ::context-child integer?)
                      (s/def ::context-parent (s/tuple ::context-child ::context-child))
                      (let [calls (atom [])
                            spec (s/tuple ::context-parent ::context-parent)]
                        (sc/def* ::context-parent
                          (fn [overrides path rmap]
                            (swap! calls conj [path (::s/recursion-limit rmap)])
                            (sc/gen* (s/get-spec ::context-parent)
                                     (assoc overrides (conj path 0) #(g/just 11)) path rmap)))
                        (let [generated (binding [sc/*recursion-limit* 7]
                                          (h/draw! (sc/gen spec {[0 1] #(g/just 12)
                                                                 [1 1] #(g/just 13)})))]
                          (is (s/valid? spec generated))
                          (is (= [[11 12] [11 13]] generated))
                          (is (= #{[[0] 7] [[1] 7]} (set @calls))))))))

(deftest contextual-generation-direct-and-named-dispatch-test
  (check-report (with {:test-cases 1 :seed 201 :database nil} []
                      (s/def ::context-dispatch (s/int-in 0 10))
                      (let [calls (atom 0)]
                        (sc/def* ::context-dispatch
                          (fn [overrides path rmap]
                            (if (= 1 (swap! calls inc))
                              (sc/gen* ::context-dispatch overrides path rmap)
                              (sc/gen* (s/get-spec ::context-dispatch)
                                       (assoc overrides path #(g/just 4)) path rmap))))
                        (let [generated (h/draw! (sc/gen ::context-dispatch))]
                          (is (= 4 generated))
                          (is (s/valid? ::context-dispatch generated))
                          (is (= 2 @calls)))))))

(deftest contextual-generation-recursive-siblings-test
  (check-report (with {:test-cases 1 :seed 201 :database nil} []
                      (s/def ::context-tree
                        (s/or :leaf #{:leaf} :branch (s/tuple ::context-tree ::context-tree)))
                      (let [calls (atom [])
                            tree (s/get-spec ::context-tree)]
                        (sc/def* ::context-tree
                          (fn [overrides path rmap]
                            (let [n (get rmap tree 0)]
                              (swap! calls conj [path n (::s/recursion-limit rmap)])
                              (if (>= n 2)
                                (g/just :leaf)
                                (sc/gen* tree overrides path rmap)))))
                        (let [generator (binding [sc/*recursion-limit* 4] (sc/gen ::context-tree))
                              generated (binding [sc/*recursion-limit* 20] (h/draw! generator))]
                          (is (s/valid? ::context-tree generated))
                          (is (= 7 (count @calls)))
                          (doseq [[path n limit] @calls]
                            (is (= n (count (filter #{:branch} path))))
                            (is (= 4 limit)))
                          (is (= [1 2 4] (mapv (fn [n] (count (filter #(= n (second %)) @calls)))
                                               (range 3)))))))))

(deftest contextual-generation-alias-and-regex-test
  (check-report (with {:test-cases 1 :seed 201 :database nil} []
                      (s/def ::context-number (s/int-in 0 10))
                      (s/def ::context-alias ::context-number)
                      (s/def ::context-regex (s/cat :n ::context-number))
                      (let [calls (atom [])]
                        (sc/def* ::context-number
                          (fn [_ path _] (swap! calls conj [:number path]) (g/just 3)))
                        (sc/def* ::context-alias
                          (fn [overrides path rmap]
                            (swap! calls conj [:alias path])
                            (sc/gen* (s/get-spec ::context-alias) overrides path rmap)))
                        (sc/def* ::context-regex
                          (fn [overrides path rmap]
                            (swap! calls conj [:regex path])
                            (sc/gen* (s/get-spec ::context-regex) overrides path rmap)))
                        (let [alias-value (h/draw! (sc/gen ::context-alias))
                              regex-value (h/draw! (sc/gen ::context-regex))]
                          (is (s/valid? ::context-alias alias-value))
                          (is (s/valid? ::context-regex regex-value))
                          (is (= 1 (count (filter #(= :alias (first %)) @calls))))
                          (is (= 1 (count (filter #(= :regex (first %)) @calls))))
                          (is (some #{[:number [:n]]} @calls)))))))

(deftest contextual-generation-delayed-registry-snapshot-test
  (check-report (with {:test-cases 1 :seed 201 :database nil} []
                      (s/def ::context-delayed-child (s/int-in 0 10))
                      (s/def ::context-delayed-parent (s/tuple ::context-delayed-child))
                      (let [calls (atom [])]
                        (sc/def ::context-delayed-child (g/just 2))
                        (sc/def* ::context-delayed-parent
                          (fn [overrides path rmap]
                            (swap! calls conj (::s/recursion-limit rmap))
                            (sc/gen* (s/get-spec ::context-delayed-parent) overrides path rmap)))
                        (let [spec (s/fspec :args (s/cat) :ret ::context-delayed-parent)
                              generator (binding [sc/*recursion-limit* 3] (sc/gen spec))]
                          (is (empty? @calls))
                          (sc/def ::context-delayed-child (g/just 8))
                          (let [f (binding [sc/*recursion-limit* 30] (h/draw! generator))]
                            (is (= [2] (f)))
                            (is (s/valid? ::context-delayed-parent (f))))
                          (is (seq @calls))
                          (is (every? #{3} @calls)))))))

(deftest contextual-generation-readme-default-test
  (check-report (with {:test-cases 1 :seed 201 :database nil} []
                      (s/def ::context-x int?)
                      (s/def ::context-y string?)
                      (s/def ::context-foo (s/keys :req [::context-x ::context-y]))
                      (sc/def* ::context-foo
                        (fn [overrides path rmap]
                          (sc/gen* (s/get-spec ::context-foo)
                                   (merge {(conj path ::context-y) #(g/just "XXX-default")}
                                          overrides)
                                   path rmap)))
                      (doseq [[overrides expected] [[{} "XXX-default"]
                                                    [{::context-y #(g/just "caller")} "caller"]
                                                    [{[::context-y] #(g/just "path")} "path"]]]
                        (let [value (h/draw! (sc/gen ::context-foo overrides))]
                          (is (s/valid? ::context-foo value))
                          (is (= expected (::context-y value))))))))

(deftest contextual-generation-nested-readme-overrides-test
  (check-report
   (with {:test-cases 1 :seed 202 :database nil} []
         (s/def ::nested-x int?)
         (s/def ::nested-y string?)
         (s/def ::nested-foo (s/keys :req [::nested-x ::nested-y]))
         (sc/def* ::nested-foo
           (fn [overrides path rmap]
             (sc/gen* (s/get-spec ::nested-foo)
                      (merge {(conj path ::nested-y) #(g/just "default")} overrides)
                      path rmap)))
         (let [spec (s/keys :req [::nested-foo] :req-un [::nested-foo])]
           (doseq [[overrides qualified unqualified]
                   [[{} "default" "default"]
                    [{[::nested-foo ::nested-y] #(g/just "path")} "path" "default"]
                    [{::nested-y #(g/just "name")} "name" "name"]]]
             (let [value (h/draw! (sc/gen spec overrides))]
               (is (s/valid? spec value))
               (is (= qualified (get-in value [::nested-foo ::nested-y])))
               (is (= unqualified (get-in value [:nested-foo ::nested-y])))))))))

(defmulti contextual-branch-event :kind)

(s/def ::contextual-branch
  (s/and (s/keys :req-un [::payload]) #(= :a (:kind %))))

(defmethod contextual-branch-event :a [_] (s/get-spec ::contextual-branch))

(deftest contextual-generation-multi-spec-branch-retag-test
  (check-report
   (with {:test-cases 1 :seed 203 :database nil} []
         (s/def ::payload integer?)
         (let [calls (atom [])
               spec (s/multi-spec contextual-branch-event :kind)]
           (sc/def* ::contextual-branch
             (fn [overrides path rmap]
               (swap! calls conj path)
               (sc/gen* (s/get-spec ::contextual-branch) overrides path rmap)))
           (let [value (h/draw! (sc/gen spec {::payload #(g/just 42)}))]
             (is (s/valid? spec value))
             (is (= {:kind :a :payload 42} value))
             (is (= [[:a]] @calls)))))))

(def contextual-var-spec (s/tuple integer? string?))

(deftest contextual-generation-var-spec-delegation-test
  (check-report
   (with {:test-cases 1 :seed 204 :database nil} []
         (s/def ::contextual-var-parent (s/tuple integer? string?))
         (sc/def* ::contextual-var-parent
           (fn [overrides path rmap]
             (sc/gen* #'contextual-var-spec overrides path rmap)))
         (let [value (h/draw! (sc/gen ::contextual-var-parent
                                      {[0] #(g/just 7) [1] #(g/just "var")}))]
           (is (s/valid? ::contextual-var-parent value))
           (is (= [7 "var"] value))))))
