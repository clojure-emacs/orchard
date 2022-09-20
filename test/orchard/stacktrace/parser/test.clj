(ns orchard.stacktrace.parser.test
  (:require
   [clojure.java.io :as io]
   [orchard.stacktrace.parser :as parser]))

(defn fixture [resource]
  (str (io/file "orchard" "stacktrace" "parser" (str (name resource) ".txt"))))

(defn read-fixture [name]
  (some-> name fixture io/resource slurp))

(defn parse-fixture [name]
  (some-> name read-fixture parser/parse))

(defn stacktrace-element? [element]
  (symbol? (first element)))
