(ns specel.map-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is]]
            [hegel-clj.clojure-test :refer [with]]
            [hegel-clj.core :as h]
            [hegel-clj.generator :as g]
            [specel.core :as sc])
  (:import (dev.hegel Generator)))

(defn- map-spec-form-generator
  []
  (g/fmap (fn [[op arity value size bounds conform-keys?]]
            `(~op (s/tuple ~@(repeat arity `integer?)) ~value
                  ~@bounds ~size :conform-keys ~conform-keys?))
          (g/tuple (g/sampled-from [`s/map-of `s/every-kv])
                   (g/integer 1 12)
                   (g/sampled-from [`integer? `(s/tuple integer? boolean?)])
                   (g/integer 0 8)
                   (g/sampled-from [[:count] [:max-count] [:min-count]])
                   (g/boolean))))

(deftest composite-map-keys-test
  (doseq [seed [42 137 2026]]
    (let [result
          (with {:test-cases 300 :seed seed :database nil}
                [form (map-spec-form-generator)]
                (let [spec (eval form)
                      generator (sc/gen spec)
                      value (h/draw! generator)]
                  (is (instance? Generator generator) (pr-str form))
                  (is (s/valid? spec value) (pr-str {:form form :value value}))))]
      (is (:passed? result) (pr-str result))
      (is (<= 300 (get-in result [:statistics :valid])) (pr-str result)))))

(deftest singleton-tuple-map-keys-test
  (let [result
        (with {:test-cases 100 :seed 81 :database nil}
              [form (g/just `(s/map-of (s/tuple integer?) integer? :count 1))]
              (let [spec (eval form)
                    value (h/draw! (sc/gen spec))]
                (is (s/valid? spec value) (pr-str value))
                (is (= 1 (count value)))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))

(deftest composite-map-key-shapes-test
  (let [result
        (with {:test-cases 300 :seed 82 :database nil}
              [key-form (g/sampled-from
                         [`(s/tuple (s/tuple integer?) boolean?)
                          `(s/coll-of integer? :kind list? :count 2)
                          `(s/coll-of integer? :kind set? :count 2)
                          `(s/map-of integer? boolean? :count 2)
                          `(s/or :tuple (s/tuple integer?) :integer integer?)])
               size (g/integer 1 5)
               conform-keys? (g/boolean)]
              (let [form `(s/map-of ~key-form integer? :count ~size
                                    :conform-keys ~conform-keys?)
                    spec (eval form)
                    value (h/draw! (sc/gen spec))]
                (is (s/valid? spec value) (pr-str {:form form :value value}))
                (is (= size (count value)))))]
    (is (:passed? result) (pr-str result))
    (is (<= 300 (get-in result [:statistics :valid])) (pr-str result))))

(deftest every-kv-nonmap-kinds-test
  (doseq [kind [`vector? `list? `seq? `set? `coll? `sequential?]]
    (let [result
          (with {:test-cases 100 :seed 83 :database nil}
                [arity (g/integer 1 6)
                 size (g/integer 0 5)
                 exact? (g/boolean)]
                (let [form `(s/every-kv (s/tuple ~@(repeat arity `integer?)) integer?
                                        :kind ~kind ~(if exact? :count :max-count) ~size)
                      spec (eval form)
                      value (h/draw! (sc/gen spec))]
                  (is (s/valid? spec value) (pr-str {:form form :value value}))
                  (is (if exact? (= size (count value)) (<= (count value) size)))))]
      (is (:passed? result) (pr-str {:kind kind :result result}))
      (is (<= 100 (get-in result [:statistics :valid])) (pr-str result)))))

(deftest map-size-options-test
  (doseq [[options lower upper]
          [[[] 0 20]
           [[:gen-max 2] 0 2]
           [[:min-count 12 :gen-max 2] 12 24]
           [[:min-count 2 :max-count 5 :gen-max 1] 2 5]
           [[:count 0] 0 0]
           [[:count 4 :gen-max 1] 4 4]]]
    (let [result
          (with {:test-cases 100 :seed 84 :database nil}
                [arity (g/integer 1 4)]
                (let [form `(s/map-of (s/tuple ~@(repeat arity `integer?)) integer?
                                      ~@options)
                      spec (eval form)
                      value (h/draw! (sc/gen spec))]
                  (is (s/valid? spec value) (pr-str {:form form :value value}))
                  (is (<= lower (count value) upper))))]
      (is (:passed? result) (pr-str {:options options :result result}))
      (is (<= 100 (get-in result [:statistics :valid])) (pr-str result)))))

(deftest map-key-equality-test
  (let [result
        (with {:test-cases 200 :seed 85 :database nil}
              [size (g/integer 1 3)
               tuple? (g/boolean)]
              (let [form `(s/map-of ~(if tuple? `(s/tuple integer?) `integer?)
                                    integer? :count ~size)
                    spec (eval form)
                    keys (g/sampled-from [1 1N 2 2N 3 3N])
                    keys (if tuple? (g/fmap vector keys) keys)
                    value (h/draw! (sc/gen spec {[0] (constantly keys)}))]
                (is (s/valid? spec value) (pr-str {:form form :value value}))
                (is (= size (count value)))))]
    (is (:passed? result) (pr-str result))
    (is (<= 200 (get-in result [:statistics :valid])) (pr-str result))))

(deftest map-path-overrides-test
  (doseq [form [`(s/map-of (s/tuple integer?) integer? :count 1)
                `(s/every-kv (s/tuple integer?) integer? :count 1)
                `(s/every-kv (s/tuple integer?) integer? :kind vector? :count 1)
                `(s/every-kv (s/tuple integer?) integer? :kind list? :count 1)
                `(s/every-kv (s/tuple integer?) integer? :kind set? :count 1)]]
    (let [result
          (with {:test-cases 100 :seed 86 :database nil}
                [k (g/long)
                 v (g/long)]
                (let [spec (eval form)
                      value (h/draw! (sc/gen spec {[0 0] #(g/just k)
                                                   [1] #(g/just v)}))]
                  (is (s/valid? spec value) (pr-str {:form form :value value}))
                  (is (= {[k] v} (into {} value)))))]
      (is (:passed? result) (pr-str {:form form :result result}))
      (is (<= 100 (get-in result [:statistics :valid])) (pr-str result)))))

(deftest map-into-test
  (doseq [form [`(s/map-of (s/tuple integer?) integer? :count 3 :into {[:discard] 0})
                `(s/every-kv (s/tuple integer?) integer? :kind vector? :count 3 :into [])
                `(s/every-kv (s/tuple integer?) integer? :kind list? :count 3 :into ())
                `(s/every-kv (s/tuple integer?) integer? :kind set? :count 3 :into #{})]]
    (let [result
          (with {:test-cases 100 :seed 87 :database nil}
                [form (g/just form)]
                (let [spec (eval form)
                      value (h/draw! (sc/gen spec))]
                  (is (s/valid? spec value) (pr-str {:form form :value value}))
                  (is (= 3 (count value)))))]
      (is (:passed? result) (pr-str {:form form :result result}))
      (is (<= 100 (get-in result [:statistics :valid])) (pr-str result)))))

(defn- lexical-map-spec-form-generator
  [op family mode into?]
  (g/fmap
   (fn [[size width conform-keys? override?]]
     (let [n (gensym "n")
           lower (gensym "lower")
           upper (gensym "upper")
           cap (gensym "cap")
           target (gensym "target")
           kind (gensym "kind")
           conform (gensym "conform")
           options (case mode
                     :count [:count n :gen-max cap]
                     :minmax [:min-count lower :max-count upper :gen-max cap]
                     :gen-max [:gen-max cap]
                     :min-gen-max [:min-count lower :gen-max cap])
           [minimum maximum] (case mode
                               :count [size size]
                               :minmax [size (+ size width)]
                               :gen-max [0 width]
                               :min-gen-max [size (max width (* 2 size))])
           into-form (case family
                       clojure.core/map? `(sorted-map 9999999 -1)
                       clojure.core/vector? [[:discard -1]]
                       clojure.core/list? `(list [:discard -1])
                       clojure.core/set? #{[:discard -1]})]
       [`(let [~n ~size
               ~lower ~size
               ~upper (+ ~size ~width)
               ~cap ~width
               ~target ~into-form
               ~kind (fn [x#] (~family x#))
               ~conform ~conform-keys?]
           (~op (s/or :integer integer?) integer?
                ~@options :kind ~kind :conform-keys ~conform
                ~@(when into? [:into target])))
        minimum maximum conform-keys? override?]))
   (g/tuple (g/integer 0 5) (g/integer 0 3) (g/boolean) (g/boolean))))

(deftest lexical-map-options-test
  (doseq [op [`s/map-of `s/every-kv]
          family [`map? `vector? `list? `set?]
          mode [:count :minmax :gen-max :min-gen-max]
          into? [false true]]
    (let [result
          (with {:test-cases 40 :seed 88 :database nil}
                [[form minimum maximum conform-keys? override?]
                 (lexical-map-spec-form-generator op family mode into?)]
                (let [spec (eval form)
                      marker 123456
                      overrides (when override?
                                  {[0 :integer] #(g/long 100 100000)
                                   [1] #(g/just marker)})
                      value (h/draw! (sc/gen spec overrides))
                      conformed (s/conform spec value)
                      context (pr-str {:form form :value value})]
                  (is (s/valid? spec value) context)
                  (is (<= minimum (count value) maximum) context)
                  (is ((eval family) value) context)
                  (is (every? (comp integer? first) value) context)
                  (when override?
                    (is (every? #(<= 100 (first %) 100000) value) context)
                    (is (every? #(= marker (second %)) value) context))
                  (when (and into? (= family `map?))
                    (is (sorted? value) context)
                    (is (not (contains? value 9999999)) context))
                  (when (and (= op `s/map-of) (= family `map?))
                    (is (= (set (if conform-keys?
                                  (map #(vector :integer %) (keys value))
                                  (keys value)))
                           (set (keys conformed))) context))))]
      (is (:passed? result) (pr-str {:op op :family family :mode mode
                                     :into? into? :result result}))
      (is (<= 40 (get-in result [:statistics :valid])) (pr-str result)))))

(deftest lexical-map-count-test
  (let [result
        (with {:test-cases 100 :seed 89 :database nil}
              [n (g/integer 0 8)]
              (doseq [spec [(s/map-of keyword? int? :count n)
                            (s/every-kv keyword? int? :count n)]]
                (let [value (h/draw! (sc/gen spec))]
                  (is (s/valid? spec value) (pr-str value))
                  (is (= n (count value))))))]
    (is (:passed? result) (pr-str result))
    (is (<= 100 (get-in result [:statistics :valid])) (pr-str result))))
