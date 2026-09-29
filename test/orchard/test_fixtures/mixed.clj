(ns orchard.test-fixtures.mixed
  "Test vars run by `orchard.test-test`. Some fail on purpose, so the name
  doesn't end in -test and the regular test runner leaves them alone."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]))

(def each-fixture-calls
  "How many times the each-fixture has run."
  (atom 0))

(use-fixtures :each (fn [f]
                      (swap! each-fixture-calls inc)
                      (f)))

(deftest ^:smoke passing
  (is (= 1 1)))

(deftest failing
  (testing "a context"
    (is (= {:a 1} {:a 2}))))

(deftest erroring
  (throw (ex-info "boom" {})))

(deftest two-assertions
  (is true)
  (is true))
