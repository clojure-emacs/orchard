(ns orchard.xref-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [orchard.xref :as xref]))

(defn- fn-transitive-dep [a b]
  (* a b))

(defn- fn-dep [a b]
  (fn-transitive-dep a b))

(defn- dummy-fn [_x]
  (map #(fn-dep % 2) (filter even? (range 1 10))))

(deftest fn-deps-test
  (testing "with a fn value"
    (is (= (xref/fn-deps dummy-fn)
           #{#'clojure.core/map #'clojure.core/filter
             #'clojure.core/even? #'clojure.core/range #'orchard.xref-test/fn-dep})))
  (testing "with a var"
    (is (= (xref/fn-deps #'dummy-fn)
           #{#'clojure.core/map #'clojure.core/filter
             #'clojure.core/even? #'clojure.core/range #'orchard.xref-test/fn-dep})))
  (testing "with a symbol"
    (is (= (xref/fn-deps 'orchard.xref-test/dummy-fn)
           #{#'clojure.core/map #'clojure.core/filter
             #'clojure.core/even? #'clojure.core/range #'orchard.xref-test/fn-dep})))
  (testing "AoT compiled functions return deps"
    (is (= #{#'clojure.core/conj}
           (xref/fn-deps reverse)))))

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
    (is (contains? (into #{} (xref/fn-refs #'map)) #'orchard.xref-test/dummy-fn)))
  (testing "that usage from inside an anonymous function is found"
    (is (contains? (into #{} (xref/fn-refs #'fn-dep)) #'orchard.xref-test/dummy-fn))))

(deftest fn-transitive-deps-test
  (testing "basics"
    (let [expected #{#'orchard.xref-test/fn-deps-test #'orchard.xref-test/fn-dep #'clojure.core/even?
                     #'clojure.core/filter #'orchard.xref-test/fn-transitive-dep #'clojure.core/map
                     #'clojure.test/test-var #'clojure.core/range #'clojure.core/inc'}]
      (is (contains? expected #'orchard.xref-test/fn-transitive-dep)
          "Specifically includes `#'fn-transitive-dep`, which is a transitive dep of `#'dummy-fn` (via `#'fn-dep`)")
      (is (= expected
             (xref/fn-transitive-deps dummy-fn))))))
