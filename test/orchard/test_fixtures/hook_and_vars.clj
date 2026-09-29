(ns orchard.test-fixtures.hook-and-vars
  "A namespace that runs its test vars through `test-ns-hook`, run by
  `orchard.test-test`."
  (:require
   [clojure.test :refer [deftest is]]))

(deftest a
  (is true))

(deftest b
  (is true))

(defn test-ns-hook []
  (a)
  (b))
