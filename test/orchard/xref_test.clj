(ns orchard.xref-test
  (:require
   [clojure.test :refer :all]
   [orchard.xref :as xref]))

(defn- dummy-fn [x]
  (map #(* % 2) (filter even? (range 1 10))))

(deftest fn-deps-test
  (testing "with a fn value"
    (is (= (xref/fn-deps dummy-fn)
           #{#'clojure.core/map #'clojure.core/filter
             #'clojure.core/even? #'clojure.core/range})))
  (testing "with a var"
    (is (= (xref/fn-deps #'dummy-fn)
           #{#'clojure.core/map #'clojure.core/filter
             #'clojure.core/even? #'clojure.core/range})))
  (testing "with a symbol"
    (is (= (xref/fn-deps 'orchard.xref-test/dummy-fn)
           #{#'clojure.core/map #'clojure.core/filter
             #'clojure.core/even? #'clojure.core/range}))))

(deftest fn-refs-test
  (testing "with a fn value"
    (is (= (xref/fn-refs dummy-fn) '()))
    (is (contains? (into #{} (xref/fn-refs #'map)) #'orchard.xref-test/dummy-fn)))
  (testing "with a var"
    (is (= (xref/fn-refs #'dummy-fn) '()))
    (is (contains? (into #{} (xref/fn-refs #'map)) #'orchard.xref-test/dummy-fn)))
  (testing "with a symbol"
    (is (= (xref/fn-refs 'orchard.xref-test/dummy-fn) '()))
    (is (contains? (into #{} (xref/fn-refs #'map)) #'orchard.xref-test/dummy-fn))))
