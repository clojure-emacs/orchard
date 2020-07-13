(ns orchard.java.resource-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [orchard.java.resource :as resource]))

(deftest resource-path-tuple-test
  (is (nil? (resource/resource-path-tuple "jar:file:fake.jar!/fake/file.clj"))))

(deftest project-resources-test
  (testing "get the correct resources for the orchard project"
    (let [resources (resource/project-resources)]
      (is (= "clojuredocs/test_export.edn" (-> resources first :relpath)))
      (is (= java.net.URL (-> resources first :url class))))))
