(ns orchard.java.parser-next-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [orchard.java.parser-next :as sut]
   [orchard.test.util :as util])
  (:import
   (orchard.java DummyClass)))

(when (and util/has-enriched-classpath?
           (try
             (Class/forName "com.sun.tools.javac.tree.DCTree$DCBlockTag")
             true
             (catch Throwable _
               false)))
  (deftest source-info-test
    (is (class? DummyClass))

    (testing "file on the filesystem"
      (is (= '{:file "orchard/java/DummyClass.java",
               :doc-first-sentence-fragments
               [{:content "Class level docstring.", :type "text"}],
               :column 1,
               :line 12,
               :class orchard.java.DummyClass,
               :doc-fragments
               [{:content "Class level docstring.\n\n "
                 :type "text"}
                {:content "<pre> \n   DummyClass dc = new DummyClass();\n  </pre>",
                 :type "html"}],
               :members
               {orchard.java.DummyClass
                {[]
                 {:name orchard.java.DummyClass,
                  :type void,
                  :argtypes [],
                  :argnames [],
                  :doc nil,
                  :column 8,
                  :line 12}},
                dummyMethod
                {[]
                 {:name dummyMethod,
                  :type java.lang.String,
                  :argtypes [],
                  :argnames [],
                  :doc "Method-level docstring. @returns the string \"hello\"",
                  :column 3,
                  :line 18}}},
               :doc
               " Class level docstring.

 <pre>
   DummyClass dc = new DummyClass();
 </pre>

 @author Arne Brasseur
"}
             (dissoc (sut/source-info 'orchard.java.DummyClass)
                     :path
                     :resource-url))))

    (testing "java file in a jar"
      (let [rt-info (sut/source-info 'clojure.lang.RT)]
        (is (= {:file "clojure/lang/RT.java"}
               (select-keys rt-info [:file])))
        (is (re-find #"jar:file:/.*/.m2/repository/org/clojure/clojure/.*/clojure-.*-sources.jar!/clojure/lang/RT.java"
                     (str (:resource-url rt-info))))))))
