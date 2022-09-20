(ns orchard.stacktrace.parser
  (:refer-clojure :exclude [StackTraceElement->vec Throwable->map])
  (:require [orchard.misc :refer [safe-read-edn]]
            [orchard.stacktrace.parser.aviso :as parser.aviso]
            [orchard.stacktrace.parser.clojure :as parser.clojure]
            [orchard.stacktrace.parser.java :as parser.java]
            [orchard.stacktrace.parser.throwable :as parser.throwable]))

(defmulti parse-stacktrace
  "Parse the stacktrace in `object` produced by `product`."
  (fn [product _stacktrace] (keyword product)))

(defmethod parse-stacktrace :aviso [_ stacktrace]
  (parser.aviso/parse-stacktrace stacktrace))

(defmethod parse-stacktrace :clojure [_ stacktrace]
  (parser.clojure/parse-stacktrace stacktrace))

(defmethod parse-stacktrace :java [_ stacktrace]
  (parser.java/parse-stacktrace stacktrace))

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
          (some (fn [product]
                  (let [result (parse-stacktrace product (transformation object))]
                    (when-not (:error result)
                      result)))
                (keys (methods parse-stacktrace))))
        input-transformations))
