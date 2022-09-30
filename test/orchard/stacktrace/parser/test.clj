(ns orchard.stacktrace.parser.test
  (:require
   [clojure.java.io :as io]
   [clojure.walk :as walk])
  (:import java.util.regex.Pattern))

(defn fixture [resource]
  (str (io/file "orchard" "stacktrace" "parser" (str (name resource) ".txt"))))

(defn read-fixture [name]
  (some-> name fixture io/resource slurp))

(defn stacktrace-element? [element]
  (symbol? (first element)))

(defn stringify-regexp [x]
  (cond->> (walk/postwalk #(if (instance? Pattern %) (str %) %) x)
    (map? x) (into {})))
