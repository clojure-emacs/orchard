(ns orchard.stacktrace.parser.clojure.tagged-literal
  "Parser for stacktraces in Clojure's tagged literal format."
  {:added "0.10.1"}
  (:require [clojure.edn :as edn]
            [orchard.stacktrace.parser.util :as util]))

(def ^:private read-options
  "The options used when reading a stacktrace in EDN format."
  {:default tagged-literal :eof nil})

(def ^:private stacktrace-start-regex
  "The regular expression matching the start of a Clojure stacktrace."
  #"(?s)#error\s*\{")

(defn parse-stacktrace
  "Parse `input` as a stacktrace in Clojure's tagged literal format."
  [input]
  (try (let [s (util/seek-to-regex input stacktrace-start-regex)
             {:keys [form tag]} (edn/read-string read-options s)]
         (if (= 'error tag)
           (assoc form :stacktrace-type :clojure.tagged-literal)
           (util/error-incorrect-input input)))
       (catch Exception e
         (util/error-unsupported-input input e))))
