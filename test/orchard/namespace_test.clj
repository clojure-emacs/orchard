(ns orchard.namespace-test
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.test :refer [are deftest is testing]]
   [orchard.misc :as misc]
   [orchard.namespace :as sut]))

(deftest project-namespaces-test
  (let [all (into #{} (sut/project-namespaces))]
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
                 orchard.java.classpath-test.third-party-compat-test
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

(deftest project-ns-forms-test
  (is (map? (sut/project-ns-forms)))
  (is (contains? (into #{} (sut/project-ns-forms))
                 ['orchard.namespace-test
                  '(ns orchard.namespace-test
                     (:require
                      [clojure.java.io :as io]
                      [clojure.string :as string]
                      [clojure.test :refer [are deftest is testing]]
                      [orchard.misc :as misc]
                      [orchard.namespace :as sut]))])))

(deftest ns-form-imports-test
  (let [corpus (mapcat sut/ns-form-imports (vals (sut/project-ns-forms)))]
    (is (seq corpus))
    (doseq [s corpus]
      (is (symbol? s))
      (when-not ('#{com.sun.tools.javadoc.RootDocImpl ;; classes on the project can be JDK dependent
                    com.sun.tools.javadoc.ModifierFilter
                    com.sun.tools.javadoc.Messager
                    com.sun.tools.javadoc.JavadocTool
                    com.sun.tools.javadoc.JavadocEnter
                    com.sun.tools.javadoc.DocEnv
                    com.sun.javadoc.Type
                    com.sun.javadoc.Tag
                    com.sun.javadoc.RootDoc
                    com.sun.javadoc.Parameter
                    com.sun.javadoc.MethodDoc
                    com.sun.javadoc.FieldDoc
                    com.sun.javadoc.Doc
                    com.sun.javadoc.ConstructorDoc
                    com.sun.javadoc.ClassDoc
                    jdk.javadoc.doclet.Doclet
                    jdk.javadoc.doclet.DocletEnvironment
                    com.sun.source.tree.ClassTree
                    com.sun.tools.javac.tree.JCTree
                    com.sun.tools.javac.util.Abort
                    com.sun.tools.javac.util.Context
                    com.sun.tools.javac.util.List
                    com.sun.source.doctree.DocCommentTree}
                 s)
        (is (-> s eval class?)
            (pr-str s))))))

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
        upper (string/upper-case string)
        lower (string/lower-case string)]
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

(deftest read-ns-name-test
  (testing "Namespace parsing"
    (let [url (-> (System/getProperty "java.io.tmpdir")
                  (io/file "orchard.namespace-test.txt")
                  (io/as-url))
          uri (.toURI url)]
      (testing "of an empty file"
        (spit url "")
        (is (nil? (sut/read-ns-name uri))))
      (testing "of an unparsable file"
        (spit url "(]$@(")
        (is (nil? (sut/read-ns-name uri))))
      (testing "of non-list tokens"
        (spit url "these are (still) tokens")
        (is (nil? (sut/read-ns-name uri))))
      (testing "when tokens precede the ns form"
        (spit url "there [is a] (ns here) after all")
        (is (= 'here (sut/read-ns-name uri))))
      (testing "when multiple ns forms are present"
        (spit url "(ns ns1) (ns ns2) (ns ns3)")
        (is (= 'ns1 (sut/read-ns-name uri))))
      (testing "when ns form is invalid"
        (spit url "(ns (:require [clojure.string]))")
        (is (nil? (sut/read-ns-name uri))))
      (testing "of top-level forms only"
        (spit url "(comment (ns ns1)) (ns ns2) (ns ns3)")
        (is (= 'ns2 (sut/read-ns-name uri))))
      (testing "of namespace with read conditionals in its `ns` form"
        (is (= 'orchard.test-ns (-> "orchard/test_ns.cljc"
                                    io/resource
                                    io/as-url
                                    sut/read-ns-name))))
      (io/delete-file url))))

(deftest namespace-resolution
  (testing "Resolving"
    (let [nses '[clojure.java.io
                 clojure.string
                 clojure.test
                 orchard.misc
                 orchard.namespace
                 orchard.cljs.test-canonical-source]]
      (testing "namespace symbols to source files"
        (is (every? identity (map sut/canonical-source nses))))
      (testing "source files to namespace symbols"
        (is (= nses (map (comp sut/read-ns-name    ; src -> ns
                               sut/canonical-source) ; ns -> src
                         nses)))))))

(deftest jvm-clojure-resource-name->ns-name
  (are [input expected] (= expected
                           (sut/jvm-clojure-resource-name->ns-name input))
    "orchard/namespace_test.clj"   'orchard.namespace-test
    "orchard/namespace_teeest.clj" nil))
