(ns orchard.stacktrace.parser.clojure.tagged-literal
  "Parser for stacktraces in Clojure's tagged literal format."
  {:added "0.11.0"}
  (:require [clojure.edn :as edn]
            [orchard.stacktrace.parser.util :as util]))

(def ^:private read-options
  "The options used when reading a stacktrace in EDN format."
  {:default tagged-literal :eof nil})

(def ^:private stacktrace-start-regex
  "The regular expression matching the start of a Clojure stacktrace."
  #"(?s)#error\s*\{")

(defn- transform-trace-element
  "Normalize the stacktrace `element`."
  [element]
  (if (sequential? element)
    (let [[class method file line-number] element]
      [(some-> class symbol)
       (some-> method symbol)
       (some-> file str)
       line-number])
    element))

(defn- transform-cause
  "Normalize the stacktrace `cause`."
  [{:keys [at message trace] :as cause}]
  (cond-> cause
    (sequential? at)
    (update :at transform-trace-element)
    (and message (not (string? message)))
    (update :message str)
    (seq trace)
    (update :trace #(mapv transform-trace-element %))))

(defn- transform
  "Normalize the `stacktrace`."
  [{:keys [cause phase via trace] :as stacktrace}]
  (cond-> stacktrace
    (and cause (not (string? cause)))
    (update :cause str)
    (and phase (not (keyword? phase)))
    (update :phase (comp keyword str))
    (seq trace)
    (update :trace #(mapv transform-trace-element %))
    (seq via)
    (update :via #(mapv transform-cause %))))

(defn parse-stacktrace
  "Parse `input` as a stacktrace in Clojure's tagged literal format."
  {:added "0.11.0"}
  [input]
  (try (let [s (util/seek-to-regex input stacktrace-start-regex)
             {:keys [form tag]} (edn/read-string read-options s)]
         (if (= 'error tag)
           (assoc (transform form) :stacktrace-type :clojure.tagged-literal)
           (util/error-incorrect-input input)))
       (catch Exception e
         (util/error-unsupported-input input e))))
