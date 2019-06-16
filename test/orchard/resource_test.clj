(ns orchard.resource-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [orchard.resource :as resource]))

(deftest resource-path-tuple-test
  (is (nil? (resource/resource-path-tuple "jar:file:fake.jar!/fake/file.clj"))))

(deftest project-resources-test
  (testing "get the correct resources for the orchard project"
    (is (= "see-also.edn" (-> (resource/project-resources) first :relpath)))
    (is (= java.net.URL (-> (resource/project-resources) first :url class)))))
