(ns orchard.misc-test
  (:require
   [clojure.test :as test :refer [deftest is testing]]
   [orchard.misc :as misc]))

(deftest as-sym-test
  (is (= nil (misc/as-sym nil)))
  (is (= nil (misc/as-sym 1)))
  (is (= nil (misc/as-sym '())))
  (is (= nil (misc/as-sym [])))
  (is (= nil (misc/as-sym {})))
  (is (= nil (misc/as-sym :WAT)))
  (is (= 'WAT (misc/as-sym "WAT")))
  (is (= 'WAT (misc/as-sym 'WAT))))

(deftest update-vals-test
  (is (= (misc/update-vals inc {1 2 3 4 5 6})
         {1 3 3 5 5 7}))
  (is (= (misc/update-vals range {1 2 3 4 5 6})
         '{5 (0 1 2 3 4 5), 3 (0 1 2 3), 1 (0 1)}))
  (is (= (misc/update-vals str {:a :b :c :d :e :f})
         {:e ":f", :c ":d", :a ":b"}))
  (is (= (misc/update-vals odd? {1 2 3 4 5 6})
         {1 false 3 false 5 false})))

(deftest update-keys-test
  (is (= (misc/update-keys inc {1 2 3 4 5 6})
         {2 2, 4 4, 6 6}))
  (is (= (misc/update-keys range {1 2 3 4 5 6})
         '{(0) 2, (0 1 2) 4, (0 1 2 3 4) 6}))
  (is (= (misc/update-keys str {:a :b :c :d :e :f})
         {":a" :b, ":c" :d, ":e" :f})))

(deftest parse-java-version-test
  (is (= (misc/parse-java-version "1.8.0") 8))
  (is (= (misc/parse-java-version "11") 11))
  (is (= (misc/parse-java-version "14-ea") 14)))

(deftest macros-suffix-add-remove
  (testing "add-ns-macros"
    (is (nil? (misc/add-ns-macros nil)))
    (is (= 'mount.tools.macro$macros (misc/add-ns-macros 'mount.tools.macro))))

  (testing "remove-macros"
    (is (nil? (misc/remove-macros nil)) "it should return nil if input is nil")
    (is (= 'mount.tools.macro (misc/remove-macros 'mount.tools.macro)) "it should not change the input if no $macros")
    (is (= 'cljs.core.async/go (misc/remove-macros 'cljs.core.async$macros/go)) "it should remove $macros from a namespaced var")
    (is (= 'mount.tools.macro (misc/remove-macros 'mount.tools.macro$macros)) "it should remove $macros from a namespace")))

(deftest name-sym
  (is (nil? (misc/name-sym nil)))
  (is (= 'unqualified (misc/name-sym 'unqualified)))
  (is (= 'sym (misc/name-sym 'qualified/sym))))

(deftest namespace-sym
  (is (nil? (misc/namespace-sym nil)))
  (is (= 'unqualified (misc/namespace-sym 'unqualified)))
  (is (= 'qualified (misc/namespace-sym 'qualified/sym))))

(deftest file-ext?
  (is (misc/file-ext? (java.net.URL. "file:/tmp/foo.jar") ".jar"))
  (is (not (misc/file-ext? (java.net.URL. "file:/tmp/foo.war") ".jar")))
  (is (not (misc/file-ext? (java.net.URL. "jar:file:/tmp/foo.jar!/BOOT-INF/lib/test.jar") ".jar"))))

(deftest directory?
  (is (misc/directory? (.toURL (.toURI (java.io.File. ""))))))

(deftest call-when-resolved
  (let [f (misc/call-when-resolved 'clojure.core/identity)]
    (is (= 1 (f 1))))
  (let [f (misc/call-when-resolved 'com.example/unknown)]
    (is (nil? (f 1)))))

(deftest lazy-seq?
  (is (misc/lazy-seq? (repeat 1)))
  (is (not (misc/lazy-seq? nil)))
  (is (not (misc/lazy-seq? [1 2 3])))
  (is (not (misc/lazy-seq? :not-a-seq))))

(deftest safe-count
  (is (= 3 (misc/safe-count [1 2 3])))
  (is (nil? (misc/safe-count (repeat 1))))
  (is (nil? (misc/safe-count :not-a-seq))))

(deftest safe-read-edn
  (is (= nil (misc/safe-read-edn "[")))
  (is (= [1 2 3] (misc/safe-read-edn "[1 2 3]"))))
