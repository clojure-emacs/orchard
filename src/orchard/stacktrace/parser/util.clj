(ns orchard.stacktrace.parser.util
  (:require [clojure.string :as str]
            [instaparse.core :as insta]))

(defn error-incorrect-input
  "Return the incorrect input error."
  [input]
  {:error :incorrect
   :type :incorrect-input
   :input input})

(defn error-unsupported-input
  "Return the unsupported input error."
  [input & [exception]]
  (cond-> {:error :unsupported
           :type :input-not-supported
           :input input}
    exception (assoc :exception exception)))

(defn seek-to-regex
  "Return the first substring in `s` matching `regexp`."
  [^String s regex]
  (when-let [match (first (re-find regex s))]
    (when-let [index (str/index-of s match)]
      (.substring s index))))

(defn parse-try
  "Try to parse the `stacktrace` string with `parser` skipping garbage
  at the front of the string."
  [parser stacktrace regex]
  (loop [stacktrace stacktrace]
    (when-not (empty? stacktrace)
      (let [result (insta/parse parser stacktrace)]
        (if (insta/get-failure result)
          (let [next-stacktrace (seek-to-regex stacktrace regex)]
            (if (= stacktrace next-stacktrace)
              result
              (recur next-stacktrace)))
          result)))))

(defn parse-stacktrace
  "Parse a stacktrace with an Instaparse parser and transformations."
  [stacktrace-type parser transformations input start-regex]
  (try (let [result (parse-try parser input start-regex)
             failure (insta/get-failure result)]
         (if (or (nil? result) failure)
           (cond-> (error-incorrect-input input)
             failure (assoc :failure failure))
           (-> (insta/transform transformations result)
               (assoc :stacktrace-type stacktrace-type))))
       (catch Exception e
         (error-unsupported-input input e))))
