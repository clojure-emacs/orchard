(ns orchard.java.parser-next-test
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [matcher-combinators.matchers :as mc]
   [orchard.java :as java]
   [orchard.misc :as misc]
   [orchard.test.util :as util :refer [is+]])
  (:import
   (orchard.java DummyClass)))

(def ^:private jdk11+? (>= misc/java-api-version 11))

(when (and jdk11+? (System/getenv "CI"))
  (deftest sources-should-be-present-on-ci
    (is util/jdk-sources-present?)))

(def source-info
  (when jdk11+?
    (requiring-resolve 'orchard.java.parser-next/source-info)))

(def parse-java
  (when jdk11+?
    (requiring-resolve 'orchard.java.parser-next/parse-java)))

(when jdk11+?
  (deftest parse-java-test
    (testing "Throws an informative exception on invalid code"
      (try
        (parse-java (io/resource "orchard/java/InvalidClass.java") nil)
        (assert false)
        (catch Exception e
          (is+ {:out #"illegal start of expression"} (ex-data e)))))))

(when jdk11+?
  (deftest source-info-test
    (is (class? DummyClass))

    (testing "file on the filesystem"
      (is+ '{:doc-first-sentence-fragments
             [{:type "text", :content "Class level docstring."}],
             :column 1,
             :line 12,
             :class orchard.java.DummyClass,
             :doc-fragments
             [{:type "text", :content "Class level docstring.\n\n"}
              {:type "html",
               :content
               #"<pre> \n\s+DummyClass dc = new DummyClass\(\);\n\s+</pre>"}],
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
           (source-info 'orchard.java.DummyClass)))))

(when (and jdk11+? util/jdk-sources-present?)
  (deftest doc-fragments-test
    (is+ [{:type "text", :content "Inserts the specified element at the tail of this queue if it is
possible to do so immediately without exceeding the queue's capacity,
returning "}
          {:type "html", :content "<pre>true</pre> "}
          {:type "text", :content " upon success and throwing an\n"}
          {:type "html", :content "<pre>IllegalStateException</pre> "}
          {:type "text", :content " if this queue is full."}]
         (-> (source-info 'java.util.concurrent.ArrayBlockingQueue)
             (get-in [:members 'add '[java.lang.Object] :doc-fragments]))
         "Returns a data structure with carefully managed whitespace location")

    (is+ {:content "<i>Param</i>&nbsp;<pre>obj</pre>:&nbsp;", :type "html"}
         (-> (source-info `Thread)
             (get-in [:members 'holdsLock '[java.lang.Object] :doc-block-tags-fragments 1]))
         "Formats params correctly")

    (let [fragments (-> (source-info `String)
                        (get-in [:members
                                 'format
                                 ['java.util.Locale 'java.lang.String (symbol "java.lang.Object[]")]
                                 :doc-fragments])
                        (->> (map :content)))
          s (str/join fragments)]
      (is (seq fragments))
      (testing "Flattens links, since they can't be clicked from most Orchard clients"
        (testing s
          (is (not (str/includes? s "<a")))
          (is (not (str/includes? s "<a href"))))))))

(when (and jdk11+? util/jdk-sources-present?)
  (deftest smoke-test
    (let [exclusions '#{java.lang.Cloneable
                        java.lang.Override
                        java.lang.Deprecated
                        java.lang.SuppressWarnings}
          corpus (->> ::_
                      namespace
                      symbol
                      util/imported-classes
                      (remove exclusions)
                      (into ['java.io.File]))]
      (when (is (> (count corpus) 50))
        (doseq [class-sym corpus
                :let [info (source-info class-sym)]]
          (testing class-sym
            (is+ {:doc any?, :doc-fragments any?, :doc-first-sentence-fragments any?
                  :members (mc/via
                            #(mapcat vals (vals %))
                            (mc/all-of not-empty
                                       (mc/seq-of {:doc any?
                                                   :doc-fragments any?
                                                   :doc-first-sentence-fragments any?})))}
                 info)))))))
