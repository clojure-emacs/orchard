(ns orchard.test.util
  (:require clojure.string
            [orchard.java.source-files :as src-files]))

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
     (clojure.string/replace s# "\r\n" "\n")))
