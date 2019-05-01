(ns orchard.xref-test
  (:require
   [clojure.test :refer :all]
   [orchard.xref :as xref]))

(defn- dummy-fn [x]
  (map #(* % 2) (filter even? (range 1 10))))

(deftest fdeps-test
  (testing "with a fn value"
    (is (= (xref/fdeps dummy-fn)
           #{#'clojure.core/map #'clojure.core/filter
             #'clojure.core/even? #'clojure.core/range})))
  (testing "with a var"
    (is (= (xref/fdeps #'dummy-fn)
           #{#'clojure.core/map #'clojure.core/filter
             #'clojure.core/even? #'clojure.core/range})))
  (testing "with a symbol"
    (is (= (xref/fdeps 'orchard.xref-test/dummy-fn)
           #{#'clojure.core/map #'clojure.core/filter
             #'clojure.core/even? #'clojure.core/range}))))

(deftest xref-test
  (testing "with a var"
    (is (= (xref/xref #'dummy-fn) '()))
    (is (contains? (into #{} (xref/xref #'map)) #'orchard.xref-test/dummy-fn)))
  (testing "with a symbol"
    (is (= (xref/xref 'orchard.xref-test/dummy-fn) '()))
    (is (contains? (into #{} (xref/xref #'map)) #'orchard.xref-test/dummy-fn))))
