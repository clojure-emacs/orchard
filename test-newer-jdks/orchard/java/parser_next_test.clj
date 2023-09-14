(ns orchard.java.parser-next-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [orchard.java :as java]
   [orchard.java.parser-next :as sut]
   [orchard.test.util :as util])
  (:import
   (orchard.java DummyClass)))

(when (and util/has-enriched-classpath?
           java/parser-next-available?)
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
               [{:content "Class level docstring.", :type "text"}
                {:content "<pre> \n   DummyClass dc = new DummyClass();\n  </pre>",
                 :type "html"}],
               :members
               {orchard.java.DummyClass
                {[]
                 {:name orchard.java.DummyClass,
                  :type void,
                  :doc-first-sentence-fragments [],
                  :column 8,
                  :argtypes [],
                  :line 12,
                  :argnames [],
                  :doc-fragments [],
                  :doc nil}},
                dummyMethod
                {[]
                 {:name dummyMethod,
                  :type java.lang.String,
                  :doc-first-sentence-fragments
                  [{:content "Method-level docstring.", :type "text"}],
                  :column 3,
                  :argtypes [],
                  :line 18,
                  :argnames [],
                  :doc-fragments
                  [{:content "Method-level docstring.", :type "text"}
                   {:content "<i>returns</i>: <pre>the string \"hello\"</pre>",
                    :type "html"}],
                  :doc
                  "Method-level docstring.

 @returns the string \"hello\""}}},
               :doc
               "Class level docstring.

 <pre>
   DummyClass dc = new DummyClass();
 </pre>

 @author Arne Brasseur"}
             (dissoc (sut/source-info 'orchard.java.DummyClass)
                     :path
                     :resource-url))))

    (testing "java file in a jar"
      (let [rt-info (sut/source-info 'clojure.lang.RT)]
        (is (= {:file "clojure/lang/RT.java"}
               (select-keys rt-info [:file])))
        (is (re-find #"jar:file:/.*/.m2/repository/org/clojure/clojure/.*/clojure-.*-sources.jar!/clojure/lang/RT.java"
                     (str (:resource-url rt-info))))))))

(when (and util/has-enriched-classpath?
           java/parser-next-available?)
  (deftest smoke-test
    (let [annotations #{'java.lang.Override
                        'java.lang.Deprecated
                        'java.lang.SuppressWarnings}
          imported-classes #'java/imported-classes
          corpus (->> ::_
                      namespace
                      symbol
                      imported-classes
                      (remove annotations)
                      (into ['java.io.File]))]
      (assert (> (count corpus)
                 50))
      (doseq [class-sym corpus
              :let [{:keys [members] :as info} (sut/source-info class-sym)]]
        (testing class-sym
          (is (contains? info :doc))
          (is (contains? info :doc-fragments))
          (is (contains? info :doc-first-sentence-fragments))
          (assert (contains? info :members))
          (let [v (mapcat vals (vals members))]
            (when-not (#{`Cloneable} class-sym)
              (assert (seq v)
                      (pr-str class-sym)))
            (doseq [m v]
              (is (contains? m :doc))
              (is (contains? m :doc-fragments))
              (is (contains? m :doc-first-sentence-fragments)))))))))
