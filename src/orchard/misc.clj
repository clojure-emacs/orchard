(ns orchard.misc
  (:require [clojure.string :as str]))

(defn deep-merge
  "Merge maps recursively. When vals are not maps, last value wins."
  [& xs]
  (let [f (fn f [& xs]
            (if (every? map? xs)
              (apply merge-with f xs)
              (last xs)))]
    (apply f (filter identity xs))))

(def java-api-version
  (try
    (let [java-ver (System/getProperty "java.version")
          [major minor _] (str/split java-ver #"\.")]
      (if (> (bigint major) 1)
        major
        (or minor "7")))
    (catch Exception _ "7")))
