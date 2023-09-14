(ns orchard.java.parser-next-test
  (:require
   [clojure.string :as string]
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
  (deftest doc-fragments-test
    (is (= [{:content "Returns an estimate of the number of active threads in the current\n thread's ", :type "text"}
            {:content "<pre>java.lang.ThreadGroup</pre> ", :type "html"}
            {:content " and its\n subgroups. Recursively iterates over all subgroups in the current\n thread's thread group.\n\nThe value returned is only an estimate because the number of\n threads may change dynamically while this method traverses internal\n data structures, and might be affected by the presence of certain\n system threads. This method is intended primarily for debugging\n and monitoring purposes.\n\n", :type "text"}
            {:content "<i>Returns</i>:&nbsp;", :type "html"}
            {:content "an estimate of the number of active threads in the current\n          thread's thread group and in any other thread group that\n          has the current thread's thread group as an ancestor", :type "text"}]
           (-> `Thread
               sut/source-info
               (get-in [:members 'activeCount [] :doc-fragments])))
        "Returns a data structure with carefully managed whitespace location")

    (is (= {:content "<i>Param</i>&nbsp;<pre>obj</pre>:&nbsp;", :type "html"}
           (-> `Thread
               sut/source-info
               (get-in [:members 'holdsLock '[java.lang.Object] :doc-fragments 5])))
        "Formats params correctly")

    (is (= {:content "<i>Param</i>&nbsp;<pre>obj</pre>:&nbsp;", :type "html"}
           (-> `Thread
               sut/source-info
               (get-in [:members 'holdsLock '[java.lang.Object] :doc-fragments 5])))
        "Formats params correctly")

    (let [fragments (-> `String
                        sut/source-info
                        (get-in [:members
                                 'format
                                 ['java.util.Locale 'java.lang.String (symbol "java.lang.Object[]")]
                                 :doc-fragments])
                        (->> (map :content)))
          s (string/join fragments)]
      (assert (seq fragments))
      (testing "Flattens links, since they can't be clicked from most Orchard clients"
        (testing s
          (is (not (string/includes? s "<a")))
          (is (not (string/includes? s "<a href"))))))))

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
