(ns orchard.namespace-test
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [are deftest is testing]]
   [orchard.misc :as misc]
   [orchard.namespace :as sut]))

(deftest project-namespaces-test
  (let [all (set (sut/project-namespaces))]
    (doseq [n '#{orchard.apropos
                 orchard.apropos-test
                 orchard.cljs.analysis
                 orchard.cljs.analysis-test
                 orchard.cljs.meta
                 orchard.cljs.test-env
                 orchard.clojuredocs
                 orchard.clojuredocs-test
                 orchard.eldoc
                 orchard.eldoc-test
                 orchard.indent
                 orchard.indent-test
                 orchard.info
                 orchard.info-test
                 orchard.inspect
                 orchard.inspect-test
                 orchard.java
                 orchard.java-test
                 orchard.java.classpath
                 orchard.java.classpath-test
                 orchard.java.resource
                 orchard.java.resource-test
                 orchard.lru-map-test
                 orchard.meta
                 orchard.meta-test
                 orchard.misc
                 orchard.misc-test
                 orchard.namespace
                 orchard.namespace-test
                 orchard.query
                 orchard.query-test
                 orchard.spec
                 orchard.spec-test
                 orchard.test-macros
                 orchard.test-no-defs
                 orchard.test-ns
                 orchard.test-ns-dep
                 orchard.test.util
                 orchard.util.io
                 orchard.util.os
                 orchard.util.os-test
                 orchard.xref
                 orchard.xref-test}]
      (is (contains? all n)
          n))))

(deftest loaded-namespaces-test
  ;; If we don't pass the second arg, some cider ns will be returned
  (is (some #(re-find #".*orchard" %) (sut/loaded-namespaces)))
  ;; Shouldn't return any orchard namespaces
  (is (not-any? #(re-find #".*orchard" %)
                (sut/loaded-namespaces [".*orchard"]))))

(defn- change-case
  "Utility fn to change the case of a URL path, to help with case sensitivity
  tests that follows"
  [url]
  (let [string (str url)
        upper (str/upper-case string)
        lower (str/lower-case string)]
    (io/as-url
     (if (= string lower) upper lower))))

(deftest project-nses-ignore-case-on-windows-test
  (let [orig-project-root sut/project-root]
    (testing "Project nses is case sensitive on non Windows oses"
      (with-redefs [misc/os-windows? (constantly false)
                    sut/project-root   (change-case orig-project-root)]
        (is (not (seq (sut/project-namespaces))))))
    (testing "Project nses ignore cases on Windows oses"
      (with-redefs [misc/os-windows? (constantly true)
                    sut/project-root   (change-case orig-project-root)]
        (is (seq (sut/project-namespaces)))))))

(deftest has-tests-errors
  (is (sut/has-tests? (find-ns 'orchard.namespace-test))))

(deftest namespace-resolution
  (testing "Resolving"
    (let [nses '[clojure.java.io
                 clojure.string
                 clojure.test
                 orchard.misc
                 orchard.namespace
                 orchard.cljs.test-canonical-source]]
      (doseq [ns nses]
        (testing "namespace symbols to source files"
          (is (sut/canonical-source ns)))))))

(deftest jvm-clojure-resource-name->ns-name
  (are [input expected] (= expected
                           (sut/jvm-clojure-resource-name->ns-name input))
    "orchard/namespace_test.clj"   'orchard.namespace-test
    "orchard/namespace_teeest.clj" nil))
