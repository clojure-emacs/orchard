(ns orchard.stacktrace.parser.clojure
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def ^:private read-options
  {:default tagged-literal :eof nil})

(defn- strip-garbage
  "Strip the garbage in front of a Clojure stacktrace."
  [s]
  (if (string? s)
    (str/replace (str s) #".*#error\s*\{" "#error {")
    s))

(defn parse-stacktrace
  "Parse the `stacktrace` string in the Clojure's tagged literal format."
  [stacktrace]
  (try (let [{:keys [form tag]} (edn/read-string read-options (strip-garbage stacktrace))]
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
