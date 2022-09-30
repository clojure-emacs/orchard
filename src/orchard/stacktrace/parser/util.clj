(ns orchard.stacktrace.parser.util
  (:require [instaparse.core :as insta]))

(defn parse-try
  "Try to parse the `stacktrace` string with `parser` skipping garbage
  at the front of the string."
  [parser stacktrace]
  (or (some #(let [result (parser (.substring stacktrace %))]
               (when-not (insta/get-failure result)
                 result))
            (range 0 (count stacktrace)))
      (parser stacktrace)))
