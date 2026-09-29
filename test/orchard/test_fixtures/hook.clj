(ns orchard.test-fixtures.hook
  "A namespace whose assertions run only through `test-ns-hook`, run by
  `orchard.test-test`."
  (:require
   [clojure.test :refer [is testing]]))

(defn a-hook-driven-check []
  (testing "ran via test-ns-hook"
    (is (= :ok :ok))))

(defn test-ns-hook []
  (a-hook-driven-check))
