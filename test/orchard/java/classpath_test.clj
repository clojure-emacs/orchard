(ns orchard.java.classpath-test
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]
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
            (.getPath ^URL x)
            x)]
    (string/replace s #"/$" "")))

(deftest classpath-test
  (testing "Classpath"
    (testing "URLs are absolute file paths"
      (let [non-absolute-paths (remove #(-> ^URL % .getPath File. .isAbsolute)
                                       (cp/classpath))]
        (is (empty? non-absolute-paths) (pr-str non-absolute-paths))))
    (testing "directory paths have a trailing slash"
      (let [dir-entries (filter misc/directory? (cp/classpath))
            no-slash (remove #(-> ^URL % .getPath (.endsWith "/")) dir-entries)]
        (is (empty? no-slash))))
    (testing "contains expected entries"
      (let [project-root (System/getProperty "user.dir")
            directory-with-jar-extension (some #(when (re-find #"not-a\.jar" (.getPath ^URL %)) %)
                                               (cp/classpath))]
        (is (some #(= (str (io/file project-root "src")) %)
                  (map trim-trailing-slash (cp/classpath))))
        (is (some #(= (str (io/file project-root "test")) %)
                  (map trim-trailing-slash (cp/classpath))))
        (is (some #(re-find #".*/clojure-.*\.jar" (.getPath ^URL %))
                  (cp/classpath)))
        (is (some #(re-find #".*/clojure-.*-sources\.jar" (.getPath ^URL %))
                  (cp/classpath)))
        (testing "Directories with .jar extension"
          (is directory-with-jar-extension "Is present in the classpath")
          (is (-> directory-with-jar-extension io/file .isDirectory)
              (pr-str (io/file directory-with-jar-extension)))))))
  (testing "System classpath"
    (testing "is visible on full classpath"
      (is (set/subset?
           (set (cp/system-classpath))
           (set (cp/classpath)))))))

(deftest classpath-resources-test
  (testing "Iterating classpath resources"
    (testing "returns non-empty lists"
      ;; The non-existing .jar can get misteriously created (is it Lein?).
      ;; Work around it:
      (-> "does-not-exist.jar" File. .delete)

      (let [dev-resources-path (-> "dev-resources" File. .getAbsolutePath)
            the-classpath (cp/classpath)
            corpus (->> the-classpath
                        (filter (fn [^URL u]
                                  (let [f (-> u io/as-file)]
                                    ;; filter out intentionally non-existing files
                                    ;; (which we put in the :test classpath for reproducing certain bug)
                                    (and (-> f .exists)
                                         (not (= (.getAbsolutePath f)
                                                 ;; remove dev-resources, only present in the :dev profile:
                                                 dev-resources-path)))))))
            ^File non-existing-jar (->> the-classpath
                                        ;; Find the non-existing jar declared under the :test profile:
                                        (some #(when (-> % io/as-file str (.contains "does-not-exist.jar"))
                                                 %)))]
        (is (seq corpus) "There's something to test")
        (is non-existing-jar "The classpath includes the non-existing jar")
        (testing "Orchard will succeed even in presence of an entry in the classpath that refers to a non-existing.jar"
          (is (not (-> non-existing-jar io/as-file .exists))
              (pr-str non-existing-jar)))
        (is (seq (mapcat cp/classpath-seq corpus)))))
    (testing "returns relative paths"
      (let [entries (mapcat cp/classpath-seq (cp/classpath))
            absolute (filter #(.isAbsolute (File. ^String %)) entries)]
        (is (empty? absolute))))))

(deftest classloader-test
  (testing "Classloader hierarchy contains current classloader"
    (is (contains? (set (cp/classloaders)) (cp/context-classloader)))))
