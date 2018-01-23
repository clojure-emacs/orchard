(ns orchard.namespace-test
  (:require [clojure.test :refer :all])
  (:require [orchard.namespace :as n]))

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
