(ns orchard.test.util
  (:require [clojure.java.io :as io]))

(def jdk-sources-present?
  (boolean (or (io/resource "java/lang/Thread.java")
               (io/resource "java.base/java/lang/Thread.java"))))

(defn imported-classes [ns-sym]
  {:post [(seq %)]}
  (->> (ns-imports ns-sym)
       (map #(-> % ^Class val .getName symbol))))
