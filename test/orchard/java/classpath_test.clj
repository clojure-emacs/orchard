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
            (.getPath ^URL x)
            x)]
    (str/replace s #"/$" "")))

(deftest classpath-test
  (testing "Classpath"
    (testing "URLs are absolute file paths"
      (doseq [^URL entry (cp/classpath)]
        (is (-> entry .getPath File. .isAbsolute)
            (pr-str entry))))
    (testing "directory paths have a trailing slash"
      (doseq [^URL entry (->> (cp/classpath)
                              (filter misc/directory?))]
        (is (-> entry .getPath (.endsWith "/")))))
    (testing "contains expected entries"
      (let [project-root (System/getProperty "user.dir")
            directory-with-jar-extension (some #(re-find #"not-a\.jar" (.getPath ^URL %))
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
          (assert (-> directory-with-jar-extension io/file .isDirectory))
          (is (some? directory-with-jar-extension)
              "Is present in the classpath")))))
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
                                          (filter (fn [u]
                                                    ;; Find the non-existing jar declared under the :test profile:
                                                    (-> u io/as-file str (.contains "does-not-exist.jar"))))
                                          first)]
          (assert (seq corpus)
                  "There's something to test")
          (assert non-existing-jar
                  "The classpath includes the non-existing jar")
          (testing "Orchard will succeed even in presence of an entry in the classpath that refers to a non-existing.jar"
            (is (not (-> non-existing-jar io/as-file .exists))
                (pr-str non-existing-jar)))
          (doseq [item corpus
                  :let [entry (cp/classpath-seq item)]]
            (is (seq entry)
                (pr-str [item entry])))))
      (testing "returns relative paths"
        (doseq [^String entry (mapcat cp/classpath-seq (cp/classpath))]
          (is (not (-> entry File. .isAbsolute))))))))

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
