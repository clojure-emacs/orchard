(ns orchard.java.parser-next-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]
   [orchard.java :as java]
   [orchard.misc :as misc]
   [orchard.test.util :as util])
  (:import
   (orchard.java DummyClass)))

(when (and (System/getenv "CI") (>= misc/java-api-version 11))
  (deftest sources-should-be-present-on-ci
    (is util/jdk-sources-present?)))

(def source-info
  (when (>= misc/java-api-version 9)
    (misc/require-and-resolve 'orchard.java.parser-next/source-info)))

(def parse-java
  (when (>= misc/java-api-version 9)
    (misc/require-and-resolve 'orchard.java.parser-utils/parse-java)))

(when @@java/parser-next-available?
  (deftest parse-java-test
    (testing "Throws an informative exception on invalid code"
      (try
        (parse-java "orchard/java/InvalidClass.java" nil)
        (assert false)
        (catch Exception e
          (is (-> e ex-data :out (string/includes? "illegal start of expression"))))))))

(when @@java/parser-next-available?
  (deftest source-info-test
    (is (class? DummyClass))

    (testing "file on the filesystem"
      (is (= '{:file "orchard/java/DummyClass.java",
               :doc-first-sentence-fragments
               [{:type "text", :content "Class level docstring."}],
               :column 1,
               :line 12,
               :class orchard.java.DummyClass,
               :doc-fragments
               [{:type "text", :content "Class level docstring.\n\n"}
                {:type "html",
                 :content
                 "<pre> \n   DummyClass dc = new DummyClass();\n  </pre>"}],
               :members
               {dummyMethod
                {[]
                 {:name dummyMethod,
                  :type java.lang.String,
                  :doc-first-sentence-fragments
                  [{:type "text", :content "Method-level docstring."}],
                  :column 5,
                  :argtypes [],
                  :non-generic-argtypes []
                  :line 18,
                  :argnames [],
                  :doc-fragments
                  [{:type "text", :content "Method-level docstring."}],
                  :doc-block-tags-fragments
                  [{:content "\n", :type "text"}
                   {:content "<i>Returns</i>:&nbsp;", :type "html"}
                   {:content "the string \"hello\"", :type "text"}],
                  :doc
                  "Method-level docstring.\n\n @return the string \"hello\""}}
                orchard.java.DummyClass
                {[]
                 {:non-generic-argtypes [],
                  :name orchard.java.DummyClass,
                  :type void,
                  :doc-first-sentence-fragments [],
                  :column 8,
                  :argtypes [],
                  :line 12,
                  :argnames [],
                  :doc-fragments [],
                  :doc-block-tags-fragments [],
                  :doc nil}}},
               :doc-block-tags-fragments [],
               :doc
               "Class level docstring.\n\n <pre>\n   DummyClass dc = new DummyClass();\n </pre>\n\n @author Arne Brasseur"}
             (dissoc (source-info 'orchard.java.DummyClass)
                     :path
                     :resource-url))))

    (testing "java file in a jar"
      (let [rt-info (source-info 'clojure.lang.RT)]
        (is (= {:file "clojure/lang/RT.java"}
               (select-keys rt-info [:file])))
        (is (re-find #"jar:file:/.*/.m2/repository/org/clojure/clojure/.*/clojure-.*-sources.jar!/clojure/lang/RT.java"
                     (str (:resource-url rt-info))))))))

(when (and @@java/parser-next-available? util/jdk-sources-present?)
  (deftest doc-fragments-test
    (is (= [{:type "text", :content "Inserts the specified element at the tail of this queue if it is
possible to do so immediately without exceeding the queue's capacity,
returning "}
            {:type "html", :content "<pre>true</pre> "}
            {:type "text", :content " upon success and throwing an\n"}
            {:type "html", :content "<pre>IllegalStateException</pre> "}
            {:type "text", :content " if this queue is full."}]
           (-> 'java.util.concurrent.ArrayBlockingQueue
               source-info
               (get-in [:members 'add '[java.lang.Object] :doc-fragments])))
        "Returns a data structure with carefully managed whitespace location")

    (is (some #{{:type "text", :content " permission as well as\n"}}
              (-> `Thread
                  source-info
                  (get-in [:members 'getAllStackTraces [] :doc-fragments])))
        "A specific fragment starts with a single space and ends in a single newline")

    (is (= {:content "<i>Param</i>&nbsp;<pre>obj</pre>:&nbsp;", :type "html"}
           (-> `Thread
               source-info
               (get-in [:members 'holdsLock '[java.lang.Object] :doc-block-tags-fragments 1])))
        "Formats params correctly")

    (let [fragments (-> `String
                        source-info
                        (get-in [:members
                                 'format
                                 ['java.util.Locale 'java.lang.String (symbol "java.lang.Object[]")]
                                 :doc-fragments])
                        (->> (map :content)))
          s (string/join fragments)]
      (is (seq fragments))
      (testing "Flattens links, since they can't be clicked from most Orchard clients"
        (testing s
          (is (not (string/includes? s "<a")))
          (is (not (string/includes? s "<a href"))))))))

(when (and @@java/parser-next-available? util/jdk-sources-present?)
  (deftest smoke-test
    (let [annotations #{'java.lang.Override
                        'java.lang.Deprecated
                        'java.lang.SuppressWarnings}
          corpus (->> ::_
                      namespace
                      symbol
                      util/imported-classes
                      (remove annotations)
                      (into ['java.io.File]))]
      (assert (> (count corpus)
                 50))
      (doseq [class-sym corpus
              :let [{:keys [members] :as info} (source-info class-sym)]]
        (testing class-sym
          (is (contains? info :doc))
          (is (contains? info :doc-fragments))
          (is (contains? info :doc-first-sentence-fragments))
          (is (contains? info :members))
          (let [v (mapcat vals (vals members))]
            (when-not (#{`Cloneable} class-sym)
              (is (seq v) (pr-str class-sym)))
            (doseq [m v]
              (is (contains? m :doc))
              (is (contains? m :doc-fragments))
              (is (contains? m :doc-first-sentence-fragments)))))))))
