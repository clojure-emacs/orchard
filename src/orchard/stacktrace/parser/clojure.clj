(ns orchard.stacktrace.parser.clojure
  (:require [clojure.edn :as edn]
            [orchard.stacktrace.parser.util :as util]))

(def ^:private read-options
  "The options used when reading a stacktrace in EDN format."
  {:default tagged-literal :eof nil})

(def ^:private stacktrace-start-regex
  "The regular expression matching the start of a Clojure stacktrace."
  #"(?s)#error\s*\{")

(defn parse-stacktrace
  "Parse the `stacktrace` string in the Clojure's tagged literal format."
  [stacktrace]
  (try (let [s (util/seek-to-regex stacktrace stacktrace-start-regex)
             {:keys [form tag]} (edn/read-string read-options s)]
         (if (= 'error tag)
           (assoc form :product :clojure)
           {:error :incorrect
            :type :incorrect-input
            :input stacktrace}))
       (catch Exception e
         {:error :unsupported
          :type :input-not-supported
          :input stacktrace
          :exception e})))
