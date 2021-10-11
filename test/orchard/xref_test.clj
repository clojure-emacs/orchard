(ns orchard.xref-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [orchard.xref :as xref]))

(defn- dummy-fn [_x]
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

;; The mere presence of this var can reproduce a certain issue. See:
;; https://github.com/clojure-emacs/orchard/issues/135#issuecomment-939731698
(def xxx 'foo/bar)

;; Like the above, but programmatic, to ensure that we the presence of a .clj file named `foo`
;; won't cause a false negative:
(def yyy (symbol (str (gensym))
                 (str (gensym))))

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
