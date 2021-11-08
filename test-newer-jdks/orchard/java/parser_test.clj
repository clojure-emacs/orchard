(ns orchard.java.parser-test
  (:require
   [orchard.java.parser :as parser]
   [clojure.test :refer [deftest is testing]]))

(defn compile-class-from-source
  "Compile a java file on the classpath.
  Returns true if all went well."
  [classname]
  (let [compiler (javax.tools.ToolProvider/getSystemJavaCompiler)]
    (.. compiler
        (getTask
         nil ;; out
         nil ;; fileManager
         nil ;; diagnosticListener
         nil ;; compilerOptions
         nil ;; classnames for annotation processing
         ;; compilationUnits
         [(.. compiler
              (getStandardFileManager nil nil nil)
              (getJavaFileForInput javax.tools.StandardLocation/CLASS_PATH
                                   classname
                                   javax.tools.JavaFileObject$Kind/SOURCE))])
        call)))

(deftest source-info-test
  (is (compile-class-from-source "orchard.java.DummyClass"))

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
                                              "/test/orchard/java/DummyClass.java"))}
           (parser/source-info 'orchard.java.DummyClass))))

  (testing "java file in a jar"
    (let [rt-info (parser/source-info 'clojure.lang.RT)]
      (is (= {:file "clojure/lang/RT.java"}
             (select-keys rt-info [:file])))
      (is (re-find #"jar:file:/.*/.m2/repository/org/clojure/clojure/.*/clojure-.*-sources.jar!/clojure/lang/RT.java"
                   (str (:resource-url rt-info)))))))
