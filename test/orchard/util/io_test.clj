(ns orchard.util.io-test
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [orchard.util.io :as util.io])
  (:import
   (java.io File)
   (java.net URI)))

(def ^File project-file
  "A file that lives in the project directory (where the tests run)."
  (.getCanonicalFile (io/file "deps.edn")))

(def ^File temp-file
  "A file that lives outside of the project directory."
  (doto (File/createTempFile "orchard-util-io" ".txt")
    (.deleteOnExit)))

(defn- jar-resource
  "A resource URL that points inside a jar file."
  ^java.net.URL []
  (io/resource "clojure/core.clj"))

(deftest file-in-project?-test
  (testing "accepts Strings, Files and Paths under the working directory"
    (is (true? (util.io/file-in-project? project-file)))
    (is (true? (util.io/file-in-project? (str project-file))))
    (is (true? (util.io/file-in-project? (.toPath project-file)))))
  (testing "returns false for files outside of the working directory"
    (is (false? (util.io/file-in-project? (.getCanonicalFile temp-file))))
    (is (false? (util.io/file-in-project? (io/file "/"))))))

(deftest relativize-project-path-test
  (is (= "deps.edn" (util.io/relativize-project-path project-file)))
  (is (= (str (io/file "src" "orchard" "util" "io.clj"))
         (util.io/relativize-project-path
          (.getCanonicalFile (io/file "src/orchard/util/io.clj"))))))

(deftest url-protocol-test
  (is (= "file" (util.io/url-protocol (io/as-url project-file))))
  (is (= "jar" (util.io/url-protocol (jar-resource))))
  (is (= "http" (util.io/url-protocol (.toURL (URI. "http://example.com/"))))))

(deftest direct-url-to-file?-test
  (is (util.io/direct-url-to-file? (io/as-url project-file)))
  (is (not (util.io/direct-url-to-file? (jar-resource)))))

(deftest resource-jarfile-test
  (testing "returns the jar file containing the resource"
    (let [jar-file (util.io/resource-jarfile (jar-resource))]
      (is (instance? File jar-file))
      (is (.exists ^File jar-file))
      (is (str/ends-with? (str jar-file) ".jar"))))
  (testing "asserts that the URL uses the jar: protocol"
    (is (thrown? AssertionError
                 (util.io/resource-jarfile (io/as-url project-file))))))

(deftest resource-artifact-test
  (testing "file: URLs return the file itself"
    (is (= project-file
           (util.io/resource-artifact (io/as-url project-file)))))
  (testing "jar: URLs return the containing jar"
    (let [artifact (util.io/resource-artifact (jar-resource))]
      (is (.exists ^File artifact))
      (is (str/ends-with? (str artifact) ".jar"))))
  (testing "other protocols throw"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"can't be situated on the filesystem"
                          (util.io/resource-artifact
                           (.toURL (URI. "http://example.com/")))))))

(deftest last-modified-time-test
  (testing "Files report their own modification time"
    (is (= (.lastModified temp-file)
           (util.io/last-modified-time temp-file)))
    (is (pos? (util.io/last-modified-time temp-file))))
  (testing "file: URLs report the modification time of the file"
    (is (= (.lastModified project-file)
           (util.io/last-modified-time (io/as-url project-file)))))
  (testing "jar: URLs report the modification time of the containing jar"
    (is (= (.lastModified ^File (util.io/resource-jarfile (jar-resource)))
           (util.io/last-modified-time (jar-resource))))))
