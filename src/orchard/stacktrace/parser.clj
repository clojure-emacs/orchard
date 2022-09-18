(ns orchard.stacktrace.parser
  (:refer-clojure :exclude [StackTraceElement->vec Throwable->map])
  (:require [clojure.edn :as edn]))

(defn- read-edn
  "Read the string `s` in EDN format in s safe way."
  [s]
  (try (edn/read-string {:default tagged-literal} s)
       (catch Exception _)))

(defmulti parse-stacktrace
  "Parse the stacktrace in `object` produced by `product`."
  (fn [product _object] (keyword product)))

(defmethod parse-stacktrace :tagged-literal [product object]
  (let [{:keys [form tag]} (read-edn object)]
    (when (= 'error tag)
      (assoc form ::product product))))

;; Throwable

;; The `StackTraceElement->vec` and `Throwable->map` functions were copied from
;; Clojure, because `StackTraceElement->vec` was introduced in Clojure version
;; 1.9 and we want to support it in older Clojure versions as well.

(defn StackTraceElement->vec
  "Constructs a data representation for a StackTraceElement: [class method file line]"
  {:added "1.9"}
  [^StackTraceElement o]
  [(symbol (.getClassName o)) (symbol (.getMethodName o)) (.getFileName o) (.getLineNumber o)])

(defn Throwable->map
  "Constructs a data representation for a Throwable with keys:
    :cause - root cause message
    :phase - error phase
    :via - cause chain, with cause keys:
             :type - exception class symbol
             :message - exception message
             :data - ex-data
             :at - top stack element
    :trace - root cause stack elements"
  {:added "1.7"}
  [^Throwable o]
  (let [base (fn [^Throwable t]
               (merge {:type (symbol (.getName (class t)))}
                      (when-let [msg (.getLocalizedMessage t)]
                        {:message msg})
                      (when-let [ed (ex-data t)]
                        {:data ed})
                      (let [st (.getStackTrace t)]
                        (when (pos? (alength st))
                          {:at (StackTraceElement->vec (aget st 0))}))))
        via (loop [via [], ^Throwable t o]
              (if t
                (recur (conj via t) (.getCause t))
                via))
        ^Throwable root (peek via)]
    (merge {:via (vec (map base via))
            :trace (vec (map StackTraceElement->vec
                             (.getStackTrace (or root o))))}
           (when-let [root-msg (.getLocalizedMessage root)]
             {:cause root-msg})
           (when-let [data (ex-data root)]
             {:data data})
           (when-let [phase (-> o ex-data :clojure.error/phase)]
             {:phase phase}))))

(defmethod parse-stacktrace :throwable [product object]
  (when (instance? Throwable object)
    (assoc (Throwable->map object) ::product product)))

(defmethod parse-stacktrace :java [product object]
  (let [{:keys [form tag]} (read-edn object)]
    (when (= 'error tag)
      (assoc form ::product product))))

(defn parse
  "Parse `object` as a stacktrace by trying all implementations of
  `parse-stacktrace` and returing the first hit."
  [object]
  (some #(when-let [stacktrace (parse-stacktrace % object)]
           stacktrace)
        (keys (methods parse-stacktrace))))
