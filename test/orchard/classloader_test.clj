(ns orchard.classloader-test
  (:require
   [clojure.test :refer :all]
   [orchard.classloader :as cl]
   [orchard.java :as java]))

(deftest boot-resource-path-test
  (let [tmp-dir-name (System/getProperty "java.io.tmpdir")]
    (testing "when classpath is a jar"
      (let [tmp-jar-path "jar:file:fake/clojure.jar"]
        (try
          (System/setProperty "fake.class.path" tmp-jar-path)
          (is (some #{"file:fake/clojure.jar"}
                    (->> (cl/class-loader) .getURLs (map str))))
          (finally
            (System/clearProperty "fake.class.path")))))
    (testing "include sources when avaliable"
      (when-let [src-url (java/jdk-find "src.zip")]
        (try
          (System/setProperty "fake.class.path" tmp-dir-name)
          (is (some #{src-url} (.getURLs (cl/class-loader))))
          (finally
            (System/clearProperty "fake.class.path")))))))
