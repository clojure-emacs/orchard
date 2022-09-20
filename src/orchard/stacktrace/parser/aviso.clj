(ns orchard.stacktrace.parser.aviso
  (:require [clojure.java.io :as io]
            [instaparse.core  :as insta :refer [defparser]]
            [orchard.misc :refer [safe-read-edn]]))

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
  [:data (apply hash-map (mapcat rest args))])

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

(def ^:private transform-stacktrace
  "Transform a stacktrace node from Instaparse to Throwable->map."
  (fn [[_ & traces] [_ & causes]]
    (let [causes (reverse causes)
          traces (reverse traces)
          root (last causes)]
      {:cause (:message root)
       :data (:data root)
       :trace (vec (apply concat traces))
       :via (mapv (fn [cause trace]
                    (assoc cause :at (last trace)))
                  causes traces)})))

(defn- transform-trace
  "Transform a :trace node from Instaparse to Throwable->map."
  [& frames]
  (vec (mapcat (fn [frame]
                 (repeat (nth frame 4 1) (vec (butlast frame))))
               frames)))

(def ^:private transformations
  "The Aviso stacktrace transformations."
  {:S transform-stacktrace
   :cause transform-cause
   :class transform-class
   :data transform-data
   :data-data safe-read-edn
   :data-key keyword
   :exception transform-exception
   :file transform-file
   :frame vector
   :method transform-method
   :number transform-number
   :simple-name str
   :simple-symbol symbol
   :trace transform-trace})

(defn parse-stacktrace
  "Parse the `stacktrace` string in the Aviso format."
  [stacktrace]
  (try (let [result (parser stacktrace)]
         (if-let [failure (insta/get-failure result)]
           {:error :incorrect
            :type :incorrect-input
            :input stacktrace
            :failure failure}
           (-> (insta/transform transformations result)
               (assoc :product :aviso))))
       (catch Exception e
         {:error :unsupported
          :type :input-not-supported
          :input stacktrace
          :exception e})))
