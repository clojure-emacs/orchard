(ns orchard.lru-map-test
  (:require
   [clojure.test :refer [deftest is testing]])
  (:import
   (mx.cider.orchard LruMap)))

(deftest lru-map-test
  (dotimes [_ 1000] ;; run the tests enough times to guarantee determinism

    (let [size 2
          m (doto (LruMap. size)
              (.put 1 1)
              (.put 2 2)
              (.put 3 3))]
      (is (= {2 2 3 3}
             (into {} m))
          "Removes older entries, based on the max size"))

    (testing "The eviction policy is access-order based"
      (let [size 3
            m (doto (LruMap. size)
                (.put 1 1)
                (.put 2 2)
                (.put 3 3)
                (.get 1) ;; triger an access
                (.put 4 4))]
        (is (= {1 1 3 3 4 4}
               (into {} m))
            "Removes {2 2} based on the size and the order access")))))
