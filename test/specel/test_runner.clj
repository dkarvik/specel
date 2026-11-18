(ns specel.test-runner
  (:require [clojure.test :as t]
            [specel.collection-test]
            [specel.core-test]
            [specel.factory-test]
            [specel.map-test]
            [specel.override-test]
            [specel.recursion-test]
            [specel.runtime-test]))

(defn -main
  [& _]
  (let [{:keys [fail error]} (t/run-tests 'specel.core-test
                                          'specel.collection-test
                                          'specel.factory-test
                                          'specel.map-test
                                          'specel.override-test
                                          'specel.recursion-test
                                          'specel.runtime-test)]
    (when (pos? (+ fail error))
      (System/exit 1))))
