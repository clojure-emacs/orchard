(ns orchard.stacktrace.parser.clojure.stacktrace
  "Parser for stacktraces in the `clojure.stacktrace` format."
  {:added "0.10.1"}
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [instaparse.core  :as insta :refer [defparser]]
            [orchard.misc :as misc]
            [orchard.stacktrace.parser.util :as util]))

(def ^{:added "0.10.1" :private true}
  stacktrace-start-regex
  "The regular expression matching the start of a `clojure.stacktrace`
  formatted stacktrace."
  #"(?s)(([^\s]+):\s([^\n\r]+).*at+)")

(defparser ^{:added "0.10.1" :private true} parser
  (io/resource "orchard/stacktrace/parser/clojure.stacktrace.bnf"))

(defn- transform-data
  "Transform a :data node into the `Throwable->map` format."
  {:added "0.10.1"}
  [& data]
  (when-let [content (misc/safe-read-edn (apply str data))]
    [:data content]))

(defn- transform-stacktrace
  "Transform the :S node into the `Throwable->map` format."
  {:added "0.10.1"}
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
  {:added "0.10.1"}
  [& exceptions]
  (reduce (fn [m [k v]] (assoc m k v)) {} exceptions))

(def ^{:added "0.10.1" :private true}
  transform-file
  "Transform a :file node into the `Throwable->map` format."
  (partial apply str))

(def ^{:added "0.10.1" :private true}
  transform-class
  "Transform a :class node into the `Throwable->map` format."
  (comp symbol (partial apply str)))

(defn- transform-message
  "Transform a :message node into the `Throwable->map` format."
  {:added "0.10.1"}
  [& content]
  [:message (apply str content)])

(def ^{:added "0.10.1" :private true}
  transform-number
  "Transform a :number node into the `Throwable->map` format."
  (comp edn/read-string (partial apply str)))

(defn- transform-trace
  "Transform a :trace node into the `Throwable->map` format."
  {:added "0.10.1"}
  [& frames]
  [:trace (vec frames)])

(def ^{:added "0.10.1" :private true}
  transformations
  "The Instaparse `clojure.stacktrace` transformations."
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
  "Parse `input` as a stacktrace in `clojure.stacktrace` format."
  {:added "0.10.1"}
  [input]
  (util/parse-stacktrace parser transformations :clojure.stacktrace stacktrace-start-regex input))
