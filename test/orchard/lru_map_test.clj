(ns orchard.lru-map-test
  (:require
   [clojure.test :refer [deftest is]])
  (:import
   (mx.cider.orchard LruMap)))

(deftest lru-cache-test
  (let [size 2
        m (doto (LruMap. size)
            (.put 1 1)
            (.put 2 2)
            (.put 3 3))]
    (is (= {2 2 3 3}
           (into {} m))
        "Removes older entries, based on the max size")))
