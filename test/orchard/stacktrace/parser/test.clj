(ns orchard.stacktrace.parser.test
  (:require
   [clojure.java.io :as io]
   [clojure.walk :as walk])
  (:import java.util.regex.Pattern))

(def exceptions
  "The fixture exceptions."
  #{:boom :divide-by-zero :short})

(def formats
  "The fixture formats."
  #{:aviso :clojure.repl :clojure.stacktrace :clojure.tagged-literal :java})

(def fixtures
  "The fixture names as keywords."
  (for [exception exceptions, format formats]
    (keyword (str (name exception) "." (name format)))))

(defn fixture
  "Return the fixture path for the parser `resource`."
  [resource]
  (str (io/file "orchard" "stacktrace" "parser" (str (name resource) ".txt"))))

(defn read-fixture
  "Read the fixture `name`."
  [name]
  (some-> name fixture io/resource slurp))

(defn stacktrace-element?
  "Return true if `element` is a stacktrace element, otherwise false."
  [element]
  (let [[class method file] element]
    (and (symbol? class)
         (symbol? method)
         (string? file))))

(defn stringify-regexp
  "Post-walk `x` and replace all instances of `java.util.regex.Pattern`
  in it by applying `clojure.core/str` on them."
  [x]
  (cond->> (walk/postwalk #(if (instance? Pattern %) (str %) %) x)
    (map? x) (into {})))
