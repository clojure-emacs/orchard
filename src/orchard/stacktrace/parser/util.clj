(ns orchard.stacktrace.parser.util
  (:require [clojure.string :as str]
            [instaparse.core :as insta]))

(defn error-incorrect-input
  "Return the incorrect input error."
  [input & [failure]]
  (cond-> {:error :incorrect
           :type :incorrect-input
           :input input}
    failure (assoc :failure failure)))

(defn error-unsupported-input
  "Return the unsupported input error."
  [input & [exception]]
  (cond-> {:error :unsupported
           :type :unsupported-input
           :input input}
    exception (assoc :exception exception)))

(defn seek-to-regex
  "Return the first substring in `s` matching `regexp`."
  [^String s regex]
  (when-let [match (first (re-find regex s))]
    (when-let [index (str/index-of s match)]
      (.substring s index))))

(defn instaparse
  "Invoke `insta/parse` with `parser` and `input`.

  Returns the parsed tree on success, or a map with an :error key and
  the Instaparse :failure on error."
  [parser input]
  (let [result (try (insta/parse parser input)
                    (catch Exception e
                      (error-unsupported-input input e)))]
    (if-let [failure (insta/get-failure result)]
      (error-incorrect-input input failure)
      result)))

(defn parse-try
  "Skip over `input` to the start of `regex` and parse the rest of the
  string. Keep doing this repeatedly until the first match."
  [parser input regex]
  (if-not (string? input)
    (error-unsupported-input input)
    (let [result (instaparse parser input)]
      (or (when-not (:error result)
            result)
          (loop [input (seek-to-regex input regex)]
            (when (seq input)
              (let [result (instaparse parser input)]
                (if (:error result)
                  (let [next-input (seek-to-regex input regex)]
                    (if (= input next-input)
                      result
                      (recur next-input)))
                  result))))
          result))))

(defn parse-stacktrace
  "Parse a stacktrace with AST transformations applied and input skipped."
  [parser transformations stacktrace-type start-regex input]
  (let [result (parse-try parser input start-regex)]
    (if (:error result)
      result
      (-> (insta/transform transformations result)
          (assoc :stacktrace-type stacktrace-type)))))
