(ns orchard.stacktrace.parser.aviso
  "Parser for stacktraces in the Aviso format."
  {:added "0.10.1"}
  (:require [clojure.java.io :as io]
            [instaparse.core  :as insta :refer [defparser]]
            [orchard.misc :refer [safe-read-edn]]
            [orchard.stacktrace.parser.util :as util]))

(def ^:private stacktrace-start-regex
  "The regular expression matching the start of an Aviso stacktrace."
  #"(?s)([^\s]+\s+[^\s]+:\s+\d+[\s])")

(defparser ^:private parser
  (io/resource "orchard/stacktrace/parser/aviso.bnf"))

(def ^:private transform-class
  "Transform a :class node into the `Throwable->map` format."
  (comp symbol (partial apply str)))

(defn- transform-cause
  "Transform a :cause node into the `Throwable->map` format."
  [& args]
  (into {} args))

(defn- transform-data
  "Transform a :data node into the `Throwable->map` format."
  [& args]
  [:data (some->> args (apply hash-map))])

(defn- transform-exception
  "Transform a :exception node into the `Throwable->map` format."
  [& args]
  (into {} args))

(def ^:private transform-file
  "Transform a :file node into the `Throwable->map` format."
  (partial apply str))

(def ^:private transform-number
  "Transform a :number node into the `Throwable->map` format."
  (comp safe-read-edn (partial apply str)))

(def ^:private transform-method
  "Transform a :method node into the `Throwable->map` format."
  (comp symbol (partial apply str)))

(defn- transform-message
  "Transform a :message node into the `Throwable->map` format."
  [& content]
  [:message (apply str content)])

(def ^:private transform-stacktrace
  "Transform a stacktrace node into the `Throwable->map` format."
  (fn [[_ & traces] [_ & causes]]
    (let [causes (reverse causes)
          traces (remove empty? traces)
          root   (last causes)]
      {:cause (:message root)
       :data  (:data root)
       :trace (vec (reverse (apply concat traces)))
       :via   (vec causes)})))

(defn- transform-trace
  "Transform a :trace node into the `Throwable->map` format."
  [& frames]
  (vec (mapcat (fn [frame]
                 (if-let [n (nth frame 4 nil)]
                   (repeat n (vec (butlast frame)))
                   [frame]))
               frames)))

(def ^:private transformations
  "The Aviso stacktrace transformations."
  {:S transform-stacktrace
   :cause transform-cause
   :class transform-class
   :data transform-data
   :data-key keyword
   :data-value (comp safe-read-edn (partial apply str))
   :exception transform-exception
   :file transform-file
   :frame vector
   :message transform-message
   :method transform-method
   :number transform-number
   :simple-name str
   :simple-symbol symbol
   :trace transform-trace})

(defn parse-stacktrace
  "Parse `input` as a stacktrace in the Aviso format."
  [input]
  (util/parse-stacktrace parser transformations :aviso stacktrace-start-regex input))
