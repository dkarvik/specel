(ns specel.collection-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is]]
            [hegel-clj.clojure-test :refer [with]]
            [hegel-clj.core :as h]
            [hegel-clj.generator :as g]
            [specel.core :as sc]))

(s/def ::required integer?)
(s/def ::left integer?)
(s/def ::left-a string?)
(s/def ::left-b boolean?)
(s/def ::right integer?)
(s/def ::right-a string?)
(s/def ::right-b boolean?)
(s/def ::optional keyword?)
(s/def ::finite #{1 2})
(s/def finite-symbol #{1 2})
(s/def finite-symbol-alias 'specel.collection-test/finite-symbol)

(defn- requirement-form-generator
  []
  (g/fmap (fn [[left-op right-op reverse?]]
            (let [left `(and ::left (~left-op ::left-a ::left-b))
                  right `(and ::right (~right-op ::right-a ::right-b))]
              `(and ::required (or ~@(if reverse? [right left] [left right])))))
          (g/tuple (g/sampled-from [`and `or])
                   (g/sampled-from [`and `or])
                   (g/boolean))))

(deftest generated-logical-requirements-test
  (let [*omissions (atom #{})
        result
        (with {:test-cases 500 :seed 601 :database nil}
              [req (requirement-form-generator)
               req-un (requirement-form-generator)]
              (let [form `(s/keys :req [~req] :req-un [~req-un]
                                  :opt [::optional] :opt-un [::optional])
                    spec (eval form)
                    value (h/draw! (sc/gen spec))]
                (is (s/valid? spec value) (pr-str {:form form :value value}))
                (doseq [k [::left ::right :left :right]]
                  (when-not (contains? value k)
                    (swap! *omissions conj k)))))]
    (is (:passed? result) (pr-str result))
    (is (pos? (get-in result [:statistics :valid])) (pr-str result))
    (is (= #{::left ::right :left :right} @*omissions))))

(defn- anonymous-kind-form-generator
  [op predicate]
  (g/fmap (fn [[element size distinct? closure?]]
            (let [kind (if closure?
                         `(let [predicate# ~predicate] (fn [value#] (predicate# value#)))
                         `(fn [value#] (~predicate value#)))]
              `(~op ~element :kind ~kind :count ~size :distinct ~distinct?)))
          (g/tuple (g/sampled-from [`integer? `string? `keyword?])
                   (g/integer 0 5)
                   (g/boolean)
                   (g/boolean))))

(deftest finite-distinct-collections-test
  (let [result
        (with {:test-cases 300 :seed 603 :database nil}
              [domain (g/set {:min-size 1 :max-size 6} (g/integer -10 10))
               extra (g/integer 1 5)
               kind (g/sampled-from [`vector? `list? `set?])
               minimum? (g/boolean)]
              (let [capacity (count domain)
                    required (+ capacity extra)
                    option (if minimum? :min-count :count)
                    form `(s/coll-of ~domain :kind ~kind :distinct true ~option ~required)
                    spec (eval form)
                    error (try (sc/gen spec) nil
                               (catch clojure.lang.ExceptionInfo e e))
                    valid-spec (eval `(s/coll-of ~domain :kind ~kind :distinct true))
                    value (h/draw! (sc/gen valid-spec))]
                (is (some? error) (pr-str form))
                (is (= capacity (:capacity (ex-data error))))
                (is (= required (:min-count (ex-data error))))
                (is (s/valid? valid-spec value))
                (is (<= (count value) capacity))))]
    (is (:passed? result) (pr-str result))
    (is (<= 300 (get-in result [:statistics :valid])) (pr-str result))))

(deftest named-finite-distinct-collections-test
  (let [result
        (with {:test-cases 100 :seed 604 :database nil}
              [element (g/sampled-from [::finite `boolean? `nil? `true? `false?])
               extra (g/integer 1 5)]
              (let [capacity (if (#{::finite `boolean?} element) 2 1)
                    spec (eval `(s/coll-of ~element :distinct true :count ~(+ capacity extra)))
                    error (try (sc/gen spec) nil
                               (catch clojure.lang.ExceptionInfo e e))]
                (is (some? error))
                (is (= capacity (:capacity (ex-data error))))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest generated-anonymous-collection-kinds-test
  (doseq [op [`s/coll-of `s/every]
          predicate [`vector? `list? `set?]]
    (let [result
          (with {:test-cases 100 :seed 602 :database nil}
                [form (anonymous-kind-form-generator op predicate)]
                (let [spec (eval form)
                      value (h/draw! (sc/gen spec))]
                  (is (s/valid? spec value) (pr-str {:form form :value value}))))]
      (is (:passed? result) (pr-str {:op op :kind predicate :result result}))
      (is (pos? (get-in result [:statistics :valid])) (pr-str result)))))

(defn- lexical-collection-form-generator
  []
  (g/fmap (fn [[op predicate size option anonymous?]]
            (let [size (if (= predicate `map?) 1 size)
                  element (if (= predicate `map?) `(s/tuple keyword? integer?) `integer?)
                  kind (if anonymous? `(fn [value#] (~predicate value#)) predicate)
                  target (condp = predicate
                           `list? ()
                           `set? #{}
                           `map? {}
                           [])
                  options (case option
                            :count [:count 'n]
                            :bounds [:min-count 'n :max-count 'n]
                            :gen-max [:gen-max 'n]
                            :into [:count 'n :into 'target])]
              {:form `(let [~'n ~size ~'target '~target ~'kind ~kind]
                        (~op ~element :kind ~'kind ~@options))
               :predicate predicate :size size :option option}))
          (g/tuple (g/sampled-from [`s/coll-of `s/every])
                   (g/sampled-from [`vector? `list? `set? `map? `indexed?])
                   (g/integer 0 5)
                   (g/sampled-from [:count :bounds :gen-max :into])
                   (g/boolean))))

(deftest generated-lexical-collection-options-test
  (let [result
        (with {:test-cases 500 :seed 605 :database nil}
              [{:keys [form predicate size option]} (lexical-collection-form-generator)]
              (let [spec (eval form)
                    value (h/draw! (sc/gen spec))]
                (is (s/valid? spec value) (pr-str {:form form :value value}))
                (is ((deref (resolve predicate)) value) (pr-str form))
                (if (= option :gen-max)
                  (is (<= (count value) size) (pr-str form))
                  (is (= size (count value)) (pr-str form)))))]
    (is (:passed? result) (pr-str result))
    (is (<= 500 (get-in result [:statistics :valid])) (pr-str result))))

(deftest generated-feasible-finite-collections-test
  (let [result
        (with {:test-cases 300 :seed 606 :database nil}
              [element (g/sampled-from [::finite `finite-symbol `finite-symbol-alias
                                        `boolean? `nil? `true? `false?])
               predicate (g/sampled-from [`vector? `list? `set?])
               exact? (g/boolean)
               anonymous? (g/boolean)]
              (let [capacity (if (#{::finite `finite-symbol `finite-symbol-alias `boolean?}
                                  element) 2 1)
                    element (if (#{`finite-symbol `finite-symbol-alias} element)
                              `(quote ~element)
                              element)
                    kind (if anonymous? `(fn [value#] (~predicate value#)) predicate)
                    options (if exact? [:count 'n] [:min-count 0 :max-count 'n])
                    form `(let [~'n ~capacity ~'kind ~kind ~'unique true]
                            (s/coll-of ~element :kind ~'kind :distinct ~'unique ~@options))
                    spec (eval form)
                    value (h/draw! (sc/gen spec))]
                (is (s/valid? spec value) (pr-str {:form form :value value}))
                (is (<= (count value) capacity))
                (when exact? (is (= capacity (count value))))))]
    (is (:passed? result) (pr-str result))
    (is (<= 300 (get-in result [:statistics :valid])) (pr-str result))))

(deftest generated-impossible-finite-symbol-collections-test
  (let [result
        (with {:test-cases 100 :seed 607 :database nil}
              [element (g/sampled-from [`finite-symbol `finite-symbol-alias])
               extra (g/integer 1 5)
               option (g/sampled-from [:count :min-count])]
              (let [form `(let [~'n ~(+ 2 extra) ~'unique true]
                            (s/coll-of '~element :distinct ~'unique ~option ~'n))
                    error (try (sc/gen (eval form)) nil
                               (catch clojure.lang.ExceptionInfo e e))]
                (is (some? error) (pr-str form))
                (is (= 2 (:capacity (ex-data error))))
                (is (= (+ 2 extra) (:min-count (ex-data error))))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest generated-map-kind-nonentry-candidates-test
  (let [result
        (with {:test-cases 100 :seed 608 :database nil}
              [size (g/integer 1 5)]
              (let [form `(let [~'n ~size]
                            (s/coll-of integer? :count ~'n
                                       :kind (fn [value#] (or (vector? value#) (map? value#)))))
                    spec (eval form)
                    value (h/draw! (sc/gen spec))]
                (is (s/valid? spec value) (pr-str {:form form :value value}))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest sorted-set-targets-test
  (let [result
        (with {:test-cases 150 :seed 610 :database nil}
              [size (g/integer 0 6)
               reverse? (g/boolean)
               kind? (g/boolean)
               custom? (g/boolean)]
              (let [form `(let [target# ~(if custom?
                                           `(sorted-set-by ~'comparator)
                                           `(sorted-set))]
                            [target# (s/coll-of integer? :into target# :count ~size
                                                ~@(when kind? [:kind `sorted?]))])
                    form (if custom?
                           `(let [~'comparator (fn [a# b#]
                                                 (~(if reverse? `> `<) a# b#))]
                              ~form)
                           form)
                    [target spec] (eval form)
                    value (h/draw! (sc/gen spec))]
                (is (s/valid? spec value))
                (is (sorted? value))
                (is (= size (count value)))
                (is (identical? (.comparator ^clojure.lang.Sorted target)
                                (.comparator ^clojure.lang.Sorted value)))
                (is (= (seq (sort (if (and custom? reverse?) > <) value)) (seq value)))))]
    (is (:passed? result) (pr-str result))
    (is (<= 150 (get-in result [:statistics :valid])) (pr-str result))))

(deftest generated-map-kind-mixed-entry-candidates-test
  (let [result
        (with {:test-cases 100 :seed 609 :database nil}
              [op (g/sampled-from [`s/coll-of `s/every])
               predicate (g/sampled-from [`map? `(fn [value#] (map? value#))])]
              (let [form `(~op (s/or :scalar integer? :entry (s/tuple keyword? integer?))
                               :kind ~predicate :count 1)
                    spec (eval form)
                    value (h/draw! (sc/gen spec))]
                (is (s/valid? spec value) (pr-str {:form form :value value}))
                (is (map? value))
                (is (= 1 (count value)))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))
