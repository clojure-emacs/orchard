(ns orchard.java.classpath-test
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [orchard.java]
   [orchard.java.classpath :as cp]
   [orchard.misc :as misc])
  (:import
   (java.io File)
   (java.net URL)))

;; Simple normalization for string compare. Classpath URLs have trailing
;; slashes; classpath entries specified at JVM launch may or may not.
(defn- trim-trailing-slash [x]
  (let [s (if (instance? URL x)
            (.getPath x)
            x)]
    (str/replace s #"/$" "")))

(deftest classpath-test
  (testing "Classpath"
    (testing "URLs are absolute file paths"
      (is (every? #(.isAbsolute (File. (.getPath %)))
                  (cp/classpath))))
    (testing "directory paths have a trailing slash"
      (is (->> (cp/classpath)
               (filter misc/directory?)
               (every? #(.endsWith (.getPath %) "/")))))
    (testing "contains expected entries"
      (let [project-root (System/getProperty "user.dir")]
        (is (some #(= (str (io/file project-root "src")) %)
                  (map trim-trailing-slash (cp/classpath))))
        (is (some #(= (str (io/file project-root "test")) %)
                  (map trim-trailing-slash (cp/classpath))))
        (is (some #(re-find #".*/clojure-.*\.jar" (.getPath %))
                  (cp/classpath)))
        (is (some #(re-find #".*/clojure-.*-sources\.jar" (.getPath %))
                  (cp/classpath))))))
  (testing "System classpath"
    (testing "is set correctly"
      (is (= (set (map trim-trailing-slash (cp/system-classpath)))
             (set (map trim-trailing-slash
                       (.split (System/getProperty "java.class.path")
                               (System/getProperty "path.separator")))))))
    (testing "is visible on full classpath"
      (is (set/subset?
           (set (cp/system-classpath))
           (set (cp/classpath)))))))

(when orchard.java/add-java-sources-via-dynapath?
  (deftest classpath-resources-test
    (testing "Iterating classpath resources"
      (testing "returns non-empty lists"
        (is (every? seq (map cp/classpath-seq (cp/classpath)))))
      (testing "returns relative paths"
        (is (every? #(not (.isAbsolute (File. %)))
                    (mapcat cp/classpath-seq (cp/classpath))))))))

(when orchard.java/add-java-sources-via-dynapath?
  (deftest classloader-test
    (testing "Classloader hierarchy contains current classloader"
      (is (contains? (set (cp/classloaders)) (cp/context-classloader))))
    (testing "Classpath modification"
      (let [orig-classloaders (cp/classloaders)
            orig-classpath (cp/classpath)
            url (-> (System/getProperty "java.io.tmpdir")
                    (io/file "test.txt")
                    (io/as-url))]
        (cp/add-classpath! url)
        (testing "adds the URL"
          (is (contains? (set (cp/classpath)) url)))
        (testing "preserves prior classpath URLs"
          (is (set/subset?
               (set orig-classpath)
               (set (cp/classpath)))))
        (testing "preserves the classloader hierarchy"
          (is (set/subset?
               (set orig-classloaders)
               (set (cp/classloaders)))))))))
