(ns orchard.stacktrace.parser
  (:require [orchard.misc :refer [safe-read-edn]]
            [orchard.stacktrace.parser.aviso :as parser.aviso]
            [orchard.stacktrace.parser.clojure :as parser.clojure]
            [orchard.stacktrace.parser.java :as parser.java]
            [orchard.stacktrace.parser.pst :as parser.pst]
            [orchard.stacktrace.parser.throwable :as parser.throwable]))

(def default-parsers
  "The default stacktrace parsers."
  [parser.clojure/parse-stacktrace
   parser.java/parse-stacktrace
   parser.pst/parse-stacktrace
   parser.aviso/parse-stacktrace
   parser.throwable/parse-stacktrace])

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
   (parse stacktrace default-parsers))
  ([stacktrace {:keys [parsers input-transformations]}]
   (some (fn [transformation]
           (some (fn [parser]
                   (let [result (parser (transformation stacktrace))]
                     (when-not (:error result)
                       result)))
                 (or parsers default-parsers)))
         (or input-transformations default-input-transformations))))
