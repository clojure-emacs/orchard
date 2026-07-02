(ns orchard.xref-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [matcher-combinators.matchers :as mc]
   [orchard.test.util :refer [is+]]
   [orchard.xref :as xref]))

(defn- fn-transitive-dep [a b]
  (* a b))

(defn- fn-dep [a b]
  (fn-transitive-dep a b))

(defn- dummy-fn [_x]
  (map #(fn-dep % 2) (filter even? (range 1 10))))

(deftest fn-deps-class-test
  (is+ nil (xref/fn-deps-class nil)
       "Is nil-safe (important, as it uses `eval` which can return anything)")

  (is+ nil (xref/fn-deps-class 2)
       "Is garbage-safe (important, as it uses `eval` which can return anything)")

  (is+ #{#'clojure.core/symbol?
         #'clojure.core/keep
         #'clojure.core/into
         #'clojure.core/class?
         #'clojure.core/resolve}
       (xref/fn-deps-class (class xref/fn-deps-class))))

;; Supports #'fn-deps-test
(deftest sample-test
  (is (xref/fn-refs #'dummy-fn)))

(deftest fn-deps-test
  (testing "with a fn value"
    (is+ (mc/equals #{#'clojure.core/map #'clojure.core/filter
                      #'clojure.core/even? #'clojure.core/range #'orchard.xref-test/fn-dep})
         (xref/fn-deps dummy-fn)))

  (testing "with a var"
    (is+ (mc/equals #{#'clojure.core/map #'clojure.core/filter
                      #'clojure.core/even? #'clojure.core/range #'orchard.xref-test/fn-dep})
         (xref/fn-deps #'dummy-fn)))

  (testing "with a var that backs a deftest"
    (is+ (mc/equals #{#'orchard.xref-test/dummy-fn
                      #'clojure.test/do-report
                      #'clojure.core/cons
                      #'clojure.core/apply
                      #'orchard.xref/fn-refs
                      #'clojure.core/list})
         (xref/fn-deps #'sample-test)))

  (testing "with a symbol"
    (is+ (mc/equals #{#'clojure.core/map #'clojure.core/filter
                      #'clojure.core/even? #'clojure.core/range #'orchard.xref-test/fn-dep})
         (xref/fn-deps 'orchard.xref-test/dummy-fn)))

  (testing "AoT compiled functions return deps"
    (is+ (mc/equals #{#'clojure.core/conj})
         (xref/fn-deps reverse))))

;; The mere presence of this var can reproduce a certain issue. See:
;; https://github.com/clojure-emacs/orchard/issues/135#issuecomment-939731698
(def xxx 'foo/bar)

;; Like the above, but programmatic, to ensure that we the presence of a .clj file named `foo`
;; won't cause a false negative:
(def yyy (symbol (str (gensym))
                 (str (gensym))))

(deftest fn-transitive-deps-test
  (is+ (mc/set-embeds
        #{#'orchard.xref-test/fn-deps-test #'orchard.xref-test/fn-dep #'clojure.core/even?
          #'clojure.core/filter #'orchard.xref-test/fn-transitive-dep #'clojure.core/map
          #'clojure.test/test-var #'clojure.core/range #'clojure.core/inc'})
       (xref/fn-transitive-deps dummy-fn)))

(deftest fn-refs-test
  (testing "with a fn value"
    (is+ (mc/in-any-order [#'orchard.xref-test/fn-deps-test
                           #'orchard.xref-test/fn-transitive-deps-test
                           #'orchard.xref-test/sample-test
                           #'orchard.xref-test/fn-refs-test])
         (xref/fn-refs dummy-fn))
    (is+ (mc/embeds [#'orchard.xref-test/dummy-fn])
         (xref/fn-refs map)))

  (testing "with a var"
    (is+ (mc/in-any-order [#'orchard.xref-test/fn-deps-test
                           #'orchard.xref-test/fn-transitive-deps-test
                           #'orchard.xref-test/sample-test
                           #'orchard.xref-test/fn-refs-test])
         (xref/fn-refs #'dummy-fn))
    (is+ (mc/embeds [#'orchard.xref-test/dummy-fn])
         (xref/fn-refs #'map)))

  (testing "with a symbol"
    (is+ (mc/in-any-order [#'orchard.xref-test/fn-deps-test
                           #'orchard.xref-test/fn-transitive-deps-test
                           #'orchard.xref-test/sample-test
                           #'orchard.xref-test/fn-refs-test])
         (xref/fn-refs 'orchard.xref-test/dummy-fn))
    (is+ (mc/embeds [#'orchard.xref-test/dummy-fn])
         (xref/fn-refs 'clojure.core/map)))

  (testing "that usage from inside an anonymous function is found"
    (is+ (mc/embeds [#'orchard.xref-test/dummy-fn])
         (xref/fn-refs #'fn-dep))))

;;; Protocol and multimethod implementations

(defprotocol TestShape
  (test-area [_]))

(extend-protocol TestShape
  nil
  (test-area [_] 0))

(defrecord TestSquare [side]
  TestShape
  (test-area [_] (* side side)))

(extend-type String
  TestShape
  (test-area [s] (count s)))

(defmulti test-describe :kind)
(defmethod test-describe :circle [_] "circle")
(defmethod test-describe :square [_] "square")

(deftest protocol-impls-test
  (let [impls (xref/protocol-impls #'TestShape)
        names (set (map :name impls))]
    (testing "includes extend-type targets"
      (is (contains? names "java.lang.String")))
    (testing "includes inline defrecord implementers"
      (is (contains? names "orchard.xref_test.TestSquare")))
    (testing "resolves a source location for a record implementer"
      (is+ {:name "orchard.xref_test.TestSquare"
            :file string?
            :line number?}
           (first (filter #(= "orchard.xref_test.TestSquare" (:name %)) impls))))))

(deftest multimethod-dispatch-values-test
  (is+ [":circle" ":square"]
       (xref/multimethod-dispatch-values #'test-describe)))

(deftest type-protocols-test
  (testing "finds a protocol implemented inline by a defrecord"
    (is (contains? (set (xref/type-protocols TestSquare)) #'TestShape)))
  (testing "finds a protocol extended onto an existing class"
    (is (contains? (set (xref/type-protocols String)) #'TestShape)))
  (testing "returns nil for a non-class argument"
    (is (nil? (xref/type-protocols nil)))))

(deftest protocols-with-method-test
  (testing "finds protocols declaring a method of the given name"
    (is (contains? (set (xref/protocols-with-method "test-area")) #'TestShape)))
  (testing "returns an empty result for an unknown method name"
    (is (empty? (xref/protocols-with-method "no-such-method-xyzzy")))))
