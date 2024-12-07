(ns orchard.test.util
  (:require [orchard.java.source-files :as src-files]))

(def jdk-sources-present?
  (boolean (src-files/class->source-file-url Thread)))

(defn imported-classes [ns-sym]
  {:post [(seq %)]}
  (->> (ns-imports ns-sym)
       (map #(-> % ^Class val .getName symbol))))
