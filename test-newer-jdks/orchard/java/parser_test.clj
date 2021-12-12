(ns orchard.java.parser-test
  (:require
   [orchard.java.parser :as parser]
   [clojure.test :refer [deftest is testing]])
  (:import
   (orchard.java DummyClass)))

(deftest source-info-test
  (is (class? DummyClass))

  (testing "file on the filesystem"
    (is (= {:class 'orchard.java.DummyClass,
            :members
            '{orchard.java.DummyClass
              {[]
               {:name orchard.java.DummyClass,
                :type void,
                :argtypes [],
                :argnames [],
                :doc nil,
                :line 12,
                :column 8}},
              dummyMethod
              {[]
               {:name dummyMethod,
                :type java.lang.String,
                :argtypes [],
                :argnames [],
                :doc "Method-level docstring. @returns the string \"hello\"",
                :line 18,
                :column 3}}},
            :doc
            "Class level docstring.\n\n```\n   DummyClass dc = new DummyClass();\n```\n\n@author Arne Brasseur",
            :line 12,
            :column 1,
            :file "orchard/java/DummyClass.java"
            :resource-url (java.net.URL. (str "file:"
                                              (System/getProperty "user.dir")
                                              "/test-java/orchard/java/DummyClass.java"))}
           (dissoc (parser/source-info 'orchard.java.DummyClass)
                   :path))))

  (testing "java file in a jar"
    (let [rt-info (parser/source-info 'clojure.lang.RT)]
      (is (= {:file "clojure/lang/RT.java"}
             (select-keys rt-info [:file])))
      (is (re-find #"jar:file:/.*/.m2/repository/org/clojure/clojure/.*/clojure-.*-sources.jar!/clojure/lang/RT.java"
                   (str (:resource-url rt-info)))))))
