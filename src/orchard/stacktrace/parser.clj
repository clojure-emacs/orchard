(ns orchard.stacktrace.parser
  (:require [orchard.misc :refer [safe-read-edn]]
            [orchard.stacktrace.parser.aviso :as parser.aviso]
            [orchard.stacktrace.parser.clojure :as parser.clojure]
            [orchard.stacktrace.parser.java :as parser.java]
            [orchard.stacktrace.parser.pst :as parser.pst]
            [orchard.stacktrace.parser.throwable :as parser.throwable]))

(defmulti parse-stacktrace
  "Parse the stacktrace in `object` produced by `stacktrace-type`."
  (fn [stacktrace-type _stacktrace] (keyword stacktrace-type)))

(defmethod parse-stacktrace :aviso [_ stacktrace]
  (parser.aviso/parse-stacktrace stacktrace))

(defmethod parse-stacktrace :clojure [_ stacktrace]
  (parser.clojure/parse-stacktrace stacktrace))

(defmethod parse-stacktrace :java [_ stacktrace]
  (parser.java/parse-stacktrace stacktrace))

(defmethod parse-stacktrace :pst [_ stacktrace]
  (parser.pst/parse-stacktrace stacktrace))

(defmethod parse-stacktrace :throwable [_ stacktrace]
  (parser.throwable/parse-stacktrace stacktrace))

(def ^:private input-transformations
  [identity safe-read-edn (comp safe-read-edn safe-read-edn)])

(defn parse
  "Parse `object` as a stacktrace by trying the permutation of
  `parse-stacktrace` method implementations and
  `input-transformations`."
  [object]
  (some (fn [transformation]
          (some (fn [stacktrace-type]
                  (let [result (parse-stacktrace stacktrace-type (transformation object))]
                    (when-not (:error result)
                      result)))
                (keys (methods parse-stacktrace))))
        input-transformations))
