(ns orchard.stacktrace.parser.util
  (:require [clojure.string :as str]
            [instaparse.core :as insta]))

(defn seek-to-regex
  "Return the first substring in `s` matching `regexp`."
  [s regex]
  (when-let [match (first (re-find regex s))]
    (when-let [index (str/index-of s match)]
      (.substring s index))))

(defn parse-try
  "Try to parse the `stacktrace` string with `parser` skipping garbage
  at the front of the string."
  [parser stacktrace regex]
  (loop [stacktrace stacktrace]
    (when-not (empty? stacktrace)
      (let [result (parser stacktrace)]
        (if (insta/get-failure result)
          (recur (seek-to-regex stacktrace regex))
          result)))))
