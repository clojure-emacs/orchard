(ns orchard.stacktrace.parser.clojure-stacktrace
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [instaparse.core  :as insta :refer [defparser]]
            [orchard.misc :as misc]
            [orchard.stacktrace.parser.util :as util]))

(def ^:private stacktrace-start-regex
  "The regular expression matching the start of a `clojure.stacktrace`
  formatted stacktrace."
  #"(?s)(([^\s]+):\s([^\n\r]+).*at+)")

(defparser ^:private parser
  (io/resource "orchard/stacktrace/parser/clojure.stacktrace.bnf"))

(defn- transform-data
  "Transform a :data node into the `Throwable->map` format."
  [& data]
  (when-let [content (misc/safe-read-edn (apply str data))]
    [:data content]))

(defn- transform-stacktrace
  "Transform the :S node into the `Throwable->map` format."
  [& causes]
  (let [root (last causes)]
    {:cause (:message root)
     :data (:data root)
     :trace (:trace root)
     :via (mapv (fn [{:keys [data type message trace]}]
                  (cond-> {:at (first trace)
                           :message message
                           :type type
                           :trace trace}
                    data (assoc :data data)))
                causes)}))

(defn- transform-exception
  "Transform a :exception node into the `Throwable->map` format."
  [& exceptions]
  (reduce (fn [m [k v]] (assoc m k v)) {} exceptions))

(def ^:private transform-file
  "Transform a :file node into the `Throwable->map` format."
  (partial apply str))

(def ^:private transform-class
  "Transform a :class node into the `Throwable->map` format."
  (comp symbol (partial apply str)))

(defn- transform-message
  "Transform a :message node into the `Throwable->map` format."
  [& content]
  [:message (apply str content)])

(def ^:private transform-number
  "Transform a :number node into the `Throwable->map` format."
  (comp edn/read-string (partial apply str)))

(defn- transform-trace
  "Transform a :trace node into the `Throwable->map` format."
  [& frames]
  [:trace (vec frames)])

(def ^:private transformations
  "The Instaparse Clojure.Stacktrace transformations."
  {:S transform-stacktrace
   :call vector
   :class transform-class
   :data transform-data
   :exception transform-exception
   :file transform-file
   :message transform-message
   :method symbol
   :number transform-number
   :simple-name str
   :simple-symbol symbol
   :trace transform-trace})

(defn parse-stacktrace
  "Parse `input` as a stacktrace in the `clojure.stacktrace` format."
  [input]
  (util/parse-stacktrace :clojure.stacktrace parser transformations input stacktrace-start-regex))
