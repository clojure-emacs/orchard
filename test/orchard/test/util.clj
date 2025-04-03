(ns orchard.test.util
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]
            [matcher-combinators.test]
            [orchard.java.source-files :as src-files]))

;; matcher-combinators.test is needed for `match?`

(def jdk-sources-present?
  (boolean (src-files/class->source-file-url Thread)))

(defn imported-classes [ns-sym]
  {:post [(seq %)]}
  (->> (ns-imports ns-sym)
       (map #(-> % ^Class val .getName symbol))))

(defmacro with-out-str-rn
  "Like `with-out-str`, but replaces Windows' CR+LF with LF."
  [& body]
  `(let [s# (with-out-str ~@body)]
     (str/replace s# "\r\n" "\n")))

(defmacro is+
  "Like `is` but wraps expected value in matcher-combinators's `match?`."
  ([expected actual]
   `(is+ ~expected ~actual nil))
  ([expected actual message]
   `(is (~'match? ~expected ~actual) ~message)))
