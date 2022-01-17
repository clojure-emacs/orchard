(ns orchard.cljs.analysis-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [orchard.cljs.analysis :as sut]))

;; Covers a NPE caught in the wild: https://gist.github.com/dgtized/aa046d06c921d4cb9f7dc51ea2729459
(deftest all-ns-test
  (testing "Handles nil-valued maps gracefully"
    (is (sut/all-ns {:cljs.analyzer/namespaces {nil nil}}))))
