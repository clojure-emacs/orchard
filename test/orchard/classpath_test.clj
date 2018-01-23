(ns orchard.classpath-test
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [orchard.classpath :as sut])
  (:import java.io.File
           java.nio.file.attribute.FileAttribute
           java.nio.file.Files
           java.util.jar.JarOutputStream
           java.util.zip.ZipEntry))

(deftest classpath-boot-test
  (let [fake-paths [(System/getProperty "java.io.tmpdir")]
        fake-classpath (str/join File/pathSeparator fake-paths)]
    (testing "when fake.class.path is not set"
      (is (not= fake-classpath (map str (sut/classpath)))))
    (testing "when fake.class.path is set"
      (try
        (System/setProperty "fake.class.path" fake-classpath)
        (is (= fake-paths (map str (sut/classpath))))
        (finally
          (System/clearProperty "fake.class.path"))))))

(deftest classpath-test
  (is (set/subset? (set (-> (System/getProperty "java.class.path")
                            (str/split (re-pattern File/pathSeparator))))
                   (set (map str (sut/classpath))))))

(defn make-temp-dir
  []
  (-> (str (gensym))
      (Files/createTempDirectory (into-array FileAttribute []))
      .toFile))

(defn make-temp-jar
  []
  (let [file (File/createTempFile (str (gensym)) ".jar")]
    (with-open [out (JarOutputStream. (io/output-stream file))]
      (.putNextEntry out (ZipEntry. (str (gensym)))))
    file))

(defn make-test-classpath
  []
  (shuffle (concat (repeatedly 2 make-temp-dir)
                   (repeatedly 3 make-temp-jar))))

(deftest classpath-directories-test
  (is (= 2 (count (sut/classpath-directories (make-test-classpath))))))

(deftest classpath-jarfiles-test
  (is (= 3 (count (sut/classpath-jarfiles (make-test-classpath))))))
