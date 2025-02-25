(ns orchard.java.resource-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]
   [matcher-combinators.matchers :as m]
   [orchard.java.resource :as resource]))

(require 'matcher-combinators.test) ;; for `match?`

(deftest resource-path-tuple-test
  (is (nil? (resource/resource-path-tuple "jar:file:fake.jar!/fake/file.clj"))))

(deftest project-resources-test
  (testing "get the correct resources for the orchard project"
    (let [test-export (some (fn [{:keys [relpath] :as res}]
                              (when (string/ends-with? relpath "test_export.edn")
                                res))
                            (resource/project-resources))]
      (is (match? {:relpath "clojuredocs/test_export.edn"
                   :url (m/via class java.net.URL)}
                  test-export)))))
