(ns orchard.stacktrace.parser
  (:require [orchard.misc :refer [safe-read-edn]]
            [orchard.stacktrace.parser.aviso :as aviso]
            [orchard.stacktrace.parser.clojure.repl :as clojure.repl]
            [orchard.stacktrace.parser.clojure.stacktrace :as clojure.stacktrace]
            [orchard.stacktrace.parser.clojure.tagged-literal :as clojure.tagged-literal]
            [orchard.stacktrace.parser.clojure.throwable :as clojure.throwable]
            [orchard.stacktrace.parser.java :as java]))

(def default-parsers
  "The default stacktrace parsers."
  [clojure.throwable/parse-stacktrace
   clojure.tagged-literal/parse-stacktrace
   clojure.stacktrace/parse-stacktrace
   java/parse-stacktrace
   clojure.repl/parse-stacktrace
   aviso/parse-stacktrace])

(def default-input-transformations
  "The default input transformations."
  [identity safe-read-edn (comp safe-read-edn safe-read-edn)])

(defn parse
  "Parse the `stacktrace`.

  The `stacktrace` is parsed by applying each function in
  `input-transformations` on `stacktrace` and invoking each of the
  `parsers` on the result. The first successful parse result will be
  returned, or nil if none of the parsers succeeded.

  If `parsers` or `input-transformations` are nil, `default-parsers`
  and `default-input-transformations` will be used instead."
  ([stacktrace]
   (parse stacktrace nil))
  ([stacktrace {:keys [parsers input-transformations]}]
   (some (fn [transformation]
           (some (fn [parser]
                   (let [result (parser (transformation stacktrace))]
                     (when-not (:error result)
                       result)))
                 (or parsers default-parsers)))
         (or input-transformations default-input-transformations))))
