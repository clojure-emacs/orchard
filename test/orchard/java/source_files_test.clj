(ns orchard.java.source-files-test
  (:require [clojure.test :refer [deftest is testing]]
            [orchard.java.source-files :as src-files]
            [orchard.test.util :as util]
            [orchard.util.os :as os]))

(when util/jdk-sources-present?
  (deftest class->source-file-url-test
    (is (src-files/class->source-file-url mx.cider.orchard.LruMap)) ;; classpath
    (is (src-files/class->source-file-url Thread))                  ;; JDK
    (is (src-files/class->source-file-url clojure.lang.PersistentVector)) ;; Clojure
    (is (nil? (src-files/class->source-file-url clojure.core.Eduction))))) ;; record

;; Download sources testing

(deftest test-infer-maven-coordinates
  (is (= (src-files/infer-maven-coordinates-for-class clojure.lang.Ref)
         {:artifact "clojure", :group "org.clojure", :version (clojure-version)}))

  (is (nil? (src-files/infer-maven-coordinates-for-class String))
      "JDK classes don't resolve to a Maven coordinate."))

(defn- sources-jar-file ^java.io.File [klass]
  (with-redefs [src-files/readable-file identity]
    (#'src-files/infer-sources-jar-file
     (#'src-files/find-jar-file-for-class klass))))

;; TODO: test for Clojure CLI and invoke tool at some point.
;; TODO: doesn't currently pass on Windows because it can't find "lein".
;; Probably a CI setup problem.
(when-not (= os/os-type ::os/windows)
  (deftest test-download-sources-jar-using-lein
    (let [f (sources-jar-file clojure.lang.Ref)]
      (.delete f)
      (is (not (.exists f))))

    (is (#'src-files/download-sources-using-lein
         (src-files/infer-maven-coordinates-for-class clojure.lang.Ref)))

    (is (.exists (sources-jar-file clojure.lang.Ref)))

    (testing "downloaded source jars contain actual source files"
      (is (> (count (slurp (src-files/class->source-file-url clojure.lang.Ref)))
             200)))))

(when-not (= os/os-type ::os/windows)
  (deftest test-download-sources-jar
    (let [f (sources-jar-file clojure.lang.Ref)]
      (.delete f)
      (is (not (.exists f))))

    (is (src-files/download-sources-jar-for-coordinates
         (src-files/infer-maven-coordinates-for-class clojure.lang.Ref)))

    (is (.exists (sources-jar-file clojure.lang.Ref)))

    (testing "downloaded source jars contain actual source files"
      (is (> (count (slurp (src-files/class->source-file-url clojure.lang.Ref)))
             200)))))
