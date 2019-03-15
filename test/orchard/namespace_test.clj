(ns orchard.namespace-test
  (:require
   [clojure.test :refer :all]
   [clojure.string :as str]
   [orchard.namespace :as n]
   [orchard.misc :as misc]))

;; Temporarily exclude this test under Java 9
;; See http://bit.ly/2DtfMMl for details
(deftest ^:java9-excluded project-namespaces-test
  (is (contains? (into #{} (n/project-namespaces))
                 'orchard.namespace)))

(deftest loaded-namespaces-test
  ;; If we don't pass the second arg, some cider ns will be returned
  (is (some #(re-find #".*orchard" %) (n/loaded-namespaces)))
  ;; Shouldn't return any orchard namespaces
  (is (not-any? #(re-find #".*orchard" %)
                (n/loaded-namespaces [".*orchard"]))))

(deftest project-nses-ignore-case-on-windows-test
  (let [orig-project-root n/project-root]
    (testing "Project nses is case sensitive on non Windows oses"
      (with-redefs [misc/os-windows? (constantly false)
                    n/project-root   (str/upper-case orig-project-root)]
         (is (not (seq (n/project-namespaces))))))
    (testing "Project nses ignore cases on Windows oses"
      (with-redefs [misc/os-windows? (constantly true)
                    n/project-root   (str/replace orig-project-root "orchard" "Orchard")]
        (is (seq (n/project-namespaces)))))))

(deftest has-tests-errors
  (is (n/has-tests? (find-ns 'orchard.namespace-test))))
