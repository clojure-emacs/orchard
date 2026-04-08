(ns orchard.java.resource-test
  (:require
   [clojure.test :refer [deftest testing]]
   [matcher-combinators.matchers :as mc]
   [orchard.java.resource :as resource]
   [orchard.test.util :refer [is+]]))

(deftest resource-path-tuple-test
  (is+ nil (resource/resource-path-tuple "jar:file:fake.jar!/fake/file.clj")))

(deftest project-resources-test
  (testing "get the correct resources for the orchard project"
    (is+ (mc/embeds [{:relpath "clojuredocs/test_export.edn"
                      :url (mc/via class java.net.URL)}])
         (resource/project-resources))))
