(ns orchard.print
  "Custom object printer used by development tooling like Inspector. Similar to
  Clojure's `print-method`. Main objectives:

  - honor `*print-level*` and `*print-length*` variables
  - provide sufficiently good performance
  - limit the maximum print size and stop printing after it is reached"
  {:author "Oleksandr Yakushev"
   :added "0.24"}
  (:refer-clojure :exclude [print print-str])
  (:import
   (clojure.core Eduction)
   (clojure.lang AFunction Compiler IDeref IPending IPersistentMap
                 IPersistentSet IPersistentVector Keyword Symbol TaggedLiteral
                 Var)
   (java.util List Map Map$Entry)
   (mx.cider.orchard TruncatingStringWriter
                     TruncatingStringWriter$TotalLimitExceeded))
  (:require [clojure.string :as str]
            [orchard.pp :as pp]))

(defmulti print
  (fn [x _]
    (cond
      (nil? x)                        nil
      (instance? String x)            :string
      (instance? Number x)            :scalar
      (instance? Keyword x)           :scalar
      (instance? Symbol x)            :scalar
      (instance? Map x)               :map
      (instance? IPersistentVector x) :vector
      (instance? List x)              :list
      (instance? IPersistentSet x)    :set
      (instance? Eduction x)          :list
      (instance? Var x)               :default
      (.isArray (class x))            :array
      :else                           (type x))))

(def ^:dynamic *max-atom-length*
  "Maximum length of the string written to the Writer in one call."
  Integer/MAX_VALUE)

(def ^:dynamic *max-total-length*
  "Maximum total size of the resulting string."
  Integer/MAX_VALUE)

(defn- print-coll-item
  "Print an item in the context of a collection. When printing a map, don't print
  `[]` characters around map entries."
  [^TruncatingStringWriter w, x, map?]
  (if (and map? (instance? Map$Entry x))
    (do (print (.getKey ^Map$Entry x) w)
        (.write w " ")
        (print (.getValue ^Map$Entry x) w))
    (print x w)))

(defn- print-coll
  ([w x sep prefix suffix]
   (print-coll w x sep prefix suffix false))
  ([^TruncatingStringWriter w, ^Iterable x, ^String sep, ^String prefix,
    ^String suffix, map?]
   (let [level *print-level*]
     (when-not (nil? level)
       (set! *print-level* (dec level)))
     (try
       (let [^Iterable iterable (if (instance? Iterable x) x (seq x))
             it (.iterator iterable)]
         (if (.hasNext it)
           (do (.write w prefix)
               (if (or (nil? level) (pos? level))
                 (do (print-coll-item w (.next it) map?)
                     (loop [remaining (unchecked-dec
                                       (long (or *print-length* Long/MAX_VALUE)))]
                       (when (.hasNext it)
                         (.write w sep)
                         (if (> remaining 0)
                           (do (print-coll-item w (.next it) map?)
                               (recur (unchecked-dec remaining)))
                           ;; There are more items but we reached the limit.
                           (.write w "...")))))
                 ;; Special case: ran out of nesting levels.
                 (.write w "..."))
               (.write w suffix))
           ;; Special case: collection has zero elements.
           (print-method x w)))
       (finally (when-not (nil? level)
                  (set! *print-level* level)))))))

(defmethod print nil [_ ^TruncatingStringWriter w]
  (.write w "nil"))

(defmethod print :string [^String x, ^TruncatingStringWriter w]
  (let [len (.length x)
        max-len *max-atom-length*
        truncate? (and max-len (< max-len len))
        len (if max-len (min max-len len) len)]
    (.append w \")
    (dotimes [n len]
      (let [c (.charAt x n)
            e (char-escape-string c)]
        (if e (.write w e) (.append w c))))
    (when truncate?
      (.write w "..."))
    (.append w \")))

(defmethod print :scalar [^Object x, ^TruncatingStringWriter w]
  (.write w (.toString x)))

(defmethod print :persistent-map [x w]
  (print-coll w x ", " "{" "}" true))

(defmethod print :vector [x w]
  (print-coll w x " " "[" "]"))

(defmethod print :list [x w]
  (print-coll w x " " "(" ")"))

(defmethod print :set [x w]
  (print-coll w x " " "#{" "}"))

(defmethod print :map [^Map x, w]
  (if (.isEmpty x)
    (print-method x w)
    (let [;; If the map is a Clojure map, don't take the entrySet but iterate
          ;; directly as the order might be important.
          coll (if (instance? IPersistentMap x) x (.entrySet ^Map x))]
      (print-coll w coll ", " "{" "}" true))))

(defmethod print :array [x, ^TruncatingStringWriter w]
  (let [ct (.getName (or (.getComponentType (class x)) Object))
        as-seq (seq x)]
    (.write w ct)
    (if as-seq
      (print-coll w as-seq ", " "[] {" "}")
      (.write w "[] {}"))))

(defmethod print IDeref [^IDeref x, ^TruncatingStringWriter w]
  (let [pending (and (instance? IPending x)
                     (not (.isRealized ^IPending x)))
        [ex val]
        (when-not pending
          (try [false (deref x)]
               (catch Throwable e
                 [true e])))]
    (.write w "#")
    (.write w (.getSimpleName (class x)))
    (print [(cond (or ex
                      (and (instance? clojure.lang.Agent x)
                           (agent-error x)))
                  '<failed>

                  pending '<pending>

                  :else val)]
           w)))

(defmethod print Class [x w]
  (print-method x w))

(defmethod print AFunction [x, ^TruncatingStringWriter w]
  (.write w "#function[")
  (.write w (Compiler/demunge (.getName (class x))))
  (.write w "]"))

(defmethod print TaggedLiteral [x w]
  (print-method x w))

(defmethod print Throwable [^Throwable x, ^TruncatingStringWriter w]
  (.write w "#Error[")
  (.write w (str (.getName (class x)) " "))
  (loop [cause x, msg nil]
    (if cause
      (recur (.getCause cause) (str msg (when msg ": ") (.getMessage cause)))
      (print msg w)))
  (when-let [data (not-empty (ex-data x))]
    (.write w " ")
    (print data w))
  (when-let [first-frame (first (.getStackTrace x))]
    (.write w " ")
    (print (str first-frame) w))
  (.write w "]"))

(defmethod print :default [^Object x, ^TruncatingStringWriter w]
  (.write w (.toString x)))

(defn print-str
  "Alternative implementation of `clojure.core/pr-str` which supports truncating
  intermediate items and the resulting string and short-circuiting when the
  limit is reached."
  [x]
  (let [writer (TruncatingStringWriter. *max-atom-length* *max-total-length*)]
    (try (print x writer)
         (catch TruncatingStringWriter$TotalLimitExceeded _))
    (.toString writer)))
