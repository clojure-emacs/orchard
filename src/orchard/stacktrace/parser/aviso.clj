(ns orchard.stacktrace.parser.aviso
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
  "Transform a :class node from Instaparse to Throwable->map."
  (comp symbol (partial apply str)))

(defn- transform-cause
  "Transform a :cause node from Instaparse to Throwable->map."
  [& args]
  (into {} args))

(defn- transform-data
  "Transform a :data node from Instaparse to Throwable->map."
  [& args]
  [:data (some->> args (apply hash-map))])

(defn- transform-exception
  "Transform a :exception node from Instaparse to Throwable->map."
  [& args]
  (into {} args))

(def ^:private transform-file
  "Transform a :file node from Instaparse to Throwable->map."
  (partial apply str))

(def ^:private transform-number
  "Transform a :number node from Instaparse to Throwable->map."
  (comp safe-read-edn (partial apply str)))

(def ^:private transform-method
  "Transform a :method node from Instaparse to Throwable->map."
  (comp symbol (partial apply str)))

(defn- transform-message
  "Transform a :message node from Instaparse to Throwable->map."
  [& content]
  [:message (apply str content)])

(def ^:private transform-stacktrace
  "Transform a stacktrace node from Instaparse to Throwable->map."
  (fn [[_ & traces] [_ & causes]]
    (let [causes (reverse causes)
          traces (remove empty? traces)
          root   (last causes)]
      {:cause (:message root)
       :data  (:data root)
       :trace (vec (reverse (apply concat traces)))
       :via   (vec causes)})))

(defn- transform-trace
  "Transform a :trace node from Instaparse to Throwable->map."
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
  "Parse the `stacktrace` string in the Aviso format."
  [stacktrace]
  (try (let [result (util/parse-try parser stacktrace stacktrace-start-regex)
             failure (insta/get-failure result)]
         (if (or (nil? result) failure)
           (cond-> {:error :incorrect
                    :type :incorrect-input
                    :input stacktrace}
             failure (assoc :failure failure))
           (-> (insta/transform transformations result)
               (assoc :product :aviso))))
       (catch Exception e
         {:error :unsupported
          :type :input-not-supported
          :input stacktrace
          :exception e})))
