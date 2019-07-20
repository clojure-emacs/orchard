(ns orchard.resource-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [orchard.resource :as resource]))

(deftest resource-path-tuple-test
  (is (nil? (resource/resource-path-tuple "jar:file:fake.jar!/fake/file.clj"))))

(deftest project-resources-test
  (testing "get the correct resources for the orchard project"
    (let [resources (->> (resource/project-resources)
                         (remove #(str/ends-with? (.getAbsolutePath (:root %)) "test-resources")))]
      (is (= "see-also.edn" (-> resources first :relpath)))
      (is (= java.net.URL (-> resources first :url class))))))
