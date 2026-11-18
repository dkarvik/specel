(ns specel.recursion-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is]]
            [hegel-clj.clojure-test :refer [with]]
            [hegel-clj.core :as h]
            [hegel-clj.generator :as g]
            [specel.core :as sc]))

(defn- productive-form-generator
  [ident]
  (g/sampled-from
   [`(s/coll-of ~ident :max-count 2)
    `(s/nilable (s/tuple ~ident))
    `(s/map-of keyword? ~ident :max-count 2)
    `(s/keys :opt [~ident])
    `(s/keys :opt-un [~ident])
    `(s/or :leaf integer? :branch (s/tuple ~ident ~ident))
    `(s/alt :leaf integer? :branch (s/cat :head string? :child ~ident))
    `(s/? (s/cat :head string? :child ~ident))]))

(defn- required-form-generator
  [ident]
  (g/sampled-from
   [`(s/tuple ~ident)
    `(s/keys :req [~ident])
    `(s/keys :req-un [~ident])
    `(s/coll-of ~ident :min-count 1 :max-count 2)
    `(s/map-of keyword? ~ident :min-count 1 :max-count 2)
    `(s/cat :child ~ident)
    `(s/+ ~ident)]))

(defn- eval-spec
  [ident form]
  (binding [*ns* (the-ns 'specel.recursion-test)]
    (eval `(do (s/def ~ident ~form) (s/get-spec ~ident)))))

(deftest productive-recursion-test
  (let [ident ::tree
        result (with {:test-cases 100 :seed 810 :database nil}
                     [form (productive-form-generator ident)
                      limit (g/integer 0 4)]
                     (let [spec (eval-spec ident form)
                           generator (binding [sc/*recursion-limit* limit] (sc/gen spec))
                           value (h/draw! generator)]
                       (is (s/valid? spec value) (pr-str {:form form :value value}))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest mutual-recursion-test
  (let [result (with {:test-cases 100 :seed 811 :database nil}
                     [form (productive-form-generator ::mutual-b)
                      limit (g/integer 0 3)]
                     (let [spec (eval-spec ::mutual-a form)]
                       (eval-spec ::mutual-b `(s/nilable (s/tuple ::mutual-a)))
                       (let [generator (binding [sc/*recursion-limit* limit] (sc/gen spec))
                             value (h/draw! generator)]
                         (is (s/valid? spec value) (pr-str {:form form :value value})))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest required-cycle-errors-test
  (let [result (with {:test-cases 100 :seed 812 :database nil}
                     [form (required-form-generator ::required)
                      limit (g/integer 0 4)]
                     (let [spec (eval-spec ::required form)
                           error (try
                                   (binding [sc/*recursion-limit* limit] (sc/gen spec))
                                   nil
                                   (catch clojure.lang.ExceptionInfo e e))]
                       (is (some? error) (pr-str form))
                       (is (::sc/no-finite-generator (ex-data error)) (pr-str form))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest recursive-regex-repetition-test
  (let [result (with {:test-cases 100 :seed 817 :database nil}
                     [op (g/sampled-from [`s/* `s/?])
                      limit (g/integer 0 4)]
                     (eval-spec ::regex-cycle `(s/tuple ::regex-cycle))
                     (let [spec (eval-spec ::regex-empty `(~op ::regex-cycle))
                           generator (binding [sc/*recursion-limit* limit] (sc/gen spec))
                           value (h/draw! generator)]
                       (is (empty? value))
                       (is (s/valid? spec value))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest mutual-required-cycle-errors-test
  (let [result (with {:test-cases 100 :seed 818 :database nil}
                     [form (required-form-generator ::required-b)
                      limit (g/integer 0 4)]
                     (let [spec (eval-spec ::required-a form)]
                       (eval-spec ::required-b `(s/tuple ::required-a))
                       (let [error (try
                                     (binding [sc/*recursion-limit* limit] (sc/gen spec))
                                     nil
                                     (catch clojure.lang.ExceptionInfo e e))]
                         (is (::sc/no-finite-generator (ex-data error)) (pr-str form)))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest required-key-alternatives-test
  (let [result (with {:test-cases 100 :seed 813 :database nil}
                     [unqualified? (g/boolean)
                      n (g/integer)]
                     (eval-spec ::cycle `(s/tuple ::cycle))
                     (eval-spec ::finite `integer?)
                     (let [form (if unqualified?
                                  `(s/keys :req-un [(or ::cycle ::finite)])
                                  `(s/keys :req [(or ::cycle ::finite)]))
                           spec (eval-spec ::choice form)
                           value (h/draw! (sc/gen spec {::finite #(g/just n)}))]
                       (is (s/valid? spec value))
                       (is (= {(if unqualified? :finite ::finite) n} value))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest zero-sized-unsupported-children-test
  (let [result (with {:test-cases 100 :seed 814 :database nil}
                     [form (g/sampled-from
                            [`(s/coll-of (s/conformer identity) :count 0)
                             `(s/coll-of (s/conformer identity) :max-count 0 :kind list?)
                             `(s/map-of (s/conformer identity) (s/conformer identity) :count 0)
                             `(s/every-kv (s/conformer identity) (s/conformer identity)
                                          :count 0 :kind vector?)])]
                     (let [spec (eval-spec ::empty form)
                           value (h/draw! (sc/gen spec))]
                       (is (empty? value))
                       (is (s/valid? spec value))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest recursive-descendant-overrides-test
  (let [result (with {:test-cases 100 :seed 815 :database nil}
                     [depth (g/integer 1 5)
                      n (g/integer)]
                     (let [spec (eval-spec ::overridden `(s/or :leaf integer?
                                                               :branch (s/tuple ::overridden)))
                           path (vec (mapcat identity (repeat depth [:branch 0])))
                           calls (atom 0)
                           generator (binding [sc/*recursion-limit* 1]
                                       (sc/gen spec {path #(do (swap! calls inc) (g/just n))}))
                           value (h/draw! generator)]
                       (is (pos? @calls))
                       (is (s/valid? spec value))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest productive-branches-preserve-errors-test
  (let [result (with {:test-cases 100 :seed 816 :database nil}
                     [form (g/sampled-from
                            [`(s/nilable (s/conformer identity))
                             `(s/coll-of (s/conformer identity) :max-count 2)
                             `(s/map-of keyword? (s/conformer identity) :max-count 2)
                             `(s/or :finite integer? :unsupported (s/conformer identity))])]
                     (let [spec (eval-spec ::unsupported form)]
                       (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                             #"Conformer requires a generator override"
                                             (sc/gen spec)))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))
