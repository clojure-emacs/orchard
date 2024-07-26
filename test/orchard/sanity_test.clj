(ns orchard.sanity-test
  (:require [clojure.test :refer [deftest is]]))

(deftest clojure-version-sanity-check
  (is (let [v (System/getenv "CLOJURE_VERSION")]
        (println "Running on Clojure" (clojure-version) ", expected:" v)
        (or (nil? v) (.startsWith ^String (clojure-version) v)))))
