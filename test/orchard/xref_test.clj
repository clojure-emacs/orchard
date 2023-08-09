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

;; Supports #'fn-deps-test
(deftest sample-test
  (is (some? xref/eval-lock))
  (is (some? (xref/fn-refs #'dummy-fn))))

(deftest fn-deps-test
  (testing "with a fn value"
    (is (= #{#'clojure.core/map #'clojure.core/filter
             #'clojure.core/even? #'clojure.core/range #'orchard.xref-test/fn-dep}
           (xref/fn-deps dummy-fn))))

  (testing "with a var"
    (is (= #{#'clojure.core/map #'clojure.core/filter
             #'clojure.core/even? #'clojure.core/range #'orchard.xref-test/fn-dep}
           (xref/fn-deps #'dummy-fn))))

  (testing "with a var that backs a deftest"
    (is (= #{#'orchard.xref-test/dummy-fn
             #'orchard.xref/eval-lock
             #'clojure.test/do-report
             #'clojure.core/cons
             #'clojure.core/some?
             #'clojure.core/apply
             #'orchard.xref/fn-refs
             #'clojure.core/list}
           (xref/fn-deps #'sample-test))))

  (testing "with a symbol"
    (is (= #{#'clojure.core/map #'clojure.core/filter
             #'clojure.core/even? #'clojure.core/range #'orchard.xref-test/fn-dep}
           (xref/fn-deps 'orchard.xref-test/dummy-fn))))

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

(deftest fn-transitive-deps-test
  (testing "basics"
    (let [expected #{#'orchard.xref-test/fn-deps-test #'orchard.xref-test/fn-dep #'clojure.core/even?
                     #'clojure.core/filter #'orchard.xref-test/fn-transitive-dep #'clojure.core/map
                     #'clojure.test/test-var #'clojure.core/range #'clojure.core/inc'}]
      (is (contains? expected #'orchard.xref-test/fn-transitive-dep)
          "Specifically includes `#'fn-transitive-dep`, which is a transitive dep of `#'dummy-fn` (via `#'fn-dep`)")
      (is (contains? expected #'clojure.core/inc')
          "Specifically includes `#'clojure.core/inc'`, which is a transitive dep of `#'dummy-fn`
           (via `#'clojure.core/range'`). Unlike other AoT compiled core transitive dependancies
           it gets found because its a non `:static` dependancy.")
      (is (= expected
             (xref/fn-transitive-deps dummy-fn))))))

(deftest fn-refs-test
  (testing "with a fn value"
    (is (= #{#'orchard.xref-test/fn-deps-test
             #'orchard.xref-test/fn-transitive-deps-test
             #'orchard.xref-test/sample-test
             #'orchard.xref-test/fn-refs-test}
           (set (xref/fn-refs dummy-fn))))
    (is (contains? (into #{} (xref/fn-refs #'map)) #'orchard.xref-test/dummy-fn)))

  (testing "with a var"
    (is (= #{#'orchard.xref-test/fn-deps-test
             #'orchard.xref-test/fn-transitive-deps-test
             #'orchard.xref-test/sample-test
             #'orchard.xref-test/fn-refs-test}
           (set (xref/fn-refs #'dummy-fn))))
    (is (contains? (into #{} (xref/fn-refs #'map)) #'orchard.xref-test/dummy-fn)))

  (testing "with a symbol"
    (is (= #{#'orchard.xref-test/fn-deps-test
             #'orchard.xref-test/fn-transitive-deps-test
             #'orchard.xref-test/sample-test
             #'orchard.xref-test/fn-refs-test}
           (set (xref/fn-refs 'orchard.xref-test/dummy-fn))))
    (is (contains? (into #{} (xref/fn-refs #'map)) #'orchard.xref-test/dummy-fn)))

  (testing "that usage from inside an anonymous function is found"
    (is (contains? (into #{} (xref/fn-refs #'fn-dep)) #'orchard.xref-test/dummy-fn))))
