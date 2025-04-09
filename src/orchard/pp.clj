(ns orchard.pp
  "A pretty-printer for Clojure data structures.

  Based on the algorithm described in \"Pretty-Printing, Converting List
  to Linear Structure\" by Ira Goldstein (Artificial Intelligence, Memo
  No. 279 in Massachusetts Institute of Technology A.I. Laboratory,
  February 1973)."
  {:author "Eero Helenius"
   :license "MIT"
   :git/url "https://github.com/eerohele/pp.git"})

(defn ^:private strip-ns
  "Given a (presumably qualified) ident, return an unqualified version
  of the ident."
  [x]
  (cond
    (keyword? x) (keyword nil (name x))
    (symbol? x) (symbol nil (name x))))

(defn ^:private extract-map-ns
  "Given a map, iff the keys in the map are qualified idents that share
  a namespace, return a tuple where the first item is the namespace
  name (a string) and the second item is a copy of the original map
  but with unqualified idents."
  [m]
  (when (seq m)
    (loop [m m ns nil nm {}]
      (if-some [[k v] (first m)]
        (when (qualified-ident? k)
          (let [k-ns (namespace k)]
            (when (or (nil? ns) (= ns k-ns))
              (recur (rest m) k-ns (assoc nm (strip-ns k) v)))))
        [ns nm]))))

(defmacro ^:private array?
  [x]
  `(some-> ~x class .isArray))

(defn ^:private open-delim
  "Return the opening delimiter (a string) of coll."
  ^String [coll]
  (cond
    (map? coll) "{"
    (vector? coll) "["
    (set? coll) "#{"
    (array? coll) "["
    :else "("))

(defn ^:private close-delim
  "Return the closing delimiter (a string) of coll."
  ^String [coll]
  (cond
    (map? coll) "}"
    (vector? coll) "]"
    (set? coll) "}"
    (array? coll) "]"
    :else ")"))

(defprotocol ^:private CountKeepingWriter
  (^:private write [this s]
    "Write a string into the underlying java.io.Writer while keeping
    count of the length of the strings written into the writer.")

  (^:private remaining [this]
    "Return the number of characters available on the current line.")

  (^:private nl [this]
    "Write a newline into the underlying java.io.Writer.

    Resets the number of characters allotted to the current line to
    zero."))

(defn ^:private write-into
  "Given a writer (java.io.Writer or cljs.core.IWriter) and a string,
  write the string into the writer."
  [writer s]
  (.write ^java.io.Writer writer ^String s))

(defn ^:private strlen
  "Given a string, return the length of the string.

  Since java.lang.String isn't counted?, (.length s) is faster than (count s)."
  ^long [s]
  (.length ^String s))

(defn ^:private count-keeping-writer
  "Given a java.io.Writer and an options map, wrap the java.io.Writer
  such that it becomes a CountKeepingWriter: a writer that keeps count
  of the length of the strings written into each line.

  Options:

    :max-width (long)
      Maximum line width."
  [writer opts]
  (let [max-width (:max-width opts)
        c (volatile! 0)]
    (reify CountKeepingWriter
      (write [_ s]
        (write-into writer ^String s)
        (vswap! c (fn [^long n] (unchecked-add-int n (strlen ^String s))))
        nil)
      (remaining [_]
        (unchecked-subtract-int max-width @c))
      (nl [_]
        (write-into writer "\n")
        (vreset! c 0)
        nil))))

(def ^:private reader-macros
  {'quote "'"
   'var "#'"
   'clojure.core/deref "@",
   'clojure.core/unquote "~"})

(defn ^:private record-name
  [record]
  (-> record class .getName))

(defn ^:private open-delim+form
  "Given a coll, return a tuple where the first item is the coll's
  opening delimiter and the second item is the coll.

  If *print-namespace-maps* is true, the coll is a map, and the map is
  amenable to the map namespace syntax, the open delimiter includes
  the map namespace prefix and the map keys are unqualified.

  If the coll is a record, the open delimiter includes the record name
  prefix."
  [coll]
  (if (record? coll)
    [(str "#" (record-name coll) "{") coll]
    ;; If all keys in the map share a namespace and *print-
    ;; namespace-maps* is true, print the map using map namespace
    ;; syntax (e.g. #:a{:b 1} instead of {:a/b 1}). If the map is
    ;; a record, print the map using the record syntax (e.g.
    ;; #user.R{:x 1}).
    (let [[ns ns-map]
          (when (and *print-namespace-maps* (map? coll))
            (extract-map-ns coll))

          coll (if ns ns-map coll)

          o (if ns (str "#:" ns "{") (open-delim coll))]
      [o coll])))

(defn ^:private meets-print-level?
  "Given a level (a long), return true if the level is the same as
  *print-level*."
  [level]
  (and (int? *print-level*) (= level *print-level*)))

(defprotocol ^:private Printable
  (^:private -print [this writer opts]
    "Given an object, a java.io.Writer, and an options map, write a
      string representation of the object into the writer in linear style
      (without regard to line length).

      Options:

        :level (long, default: 0)
          The current nesting level."))

(defn ^:private -print-map-entry
  "Print a map entry within a map."
  [this writer opts]
  (if (meets-print-level? (:level opts))
    (write-into writer "#")
    (let [opts (update opts :level inc)]
      (-print (key this) writer opts)
      (write-into writer " ")
      (-print (val this) writer opts))))

(defn ^:private -print-map
  "Like -print, but only for maps."
  [coll writer opts]
  (if (meets-print-level? (:level opts 0))
    (write-into writer "#")

    (let [[^String o form] (open-delim+form coll)]
      (write-into writer o)

      (when (seq form)
        (loop [form form index 0]
          (if (= index *print-length*)
            (write-into writer "...")
            (let [f (first form)
                  n (next form)]
              (-print-map-entry f writer (update opts :level inc))
              (when-not (empty? n)
                (write-into writer ^String (:map-entry-separator opts))
                (write-into writer " ")
                (recur n (inc index)))))))

      (write-into writer (close-delim form)))))

(defn ^:private -print-coll
  "Like -print, but only for lists, vectors, and sets."
  [coll writer opts]
  (if (meets-print-level? (:level opts 0))
    (write-into writer "#")

    (let [[^String o form] (open-delim+form coll)]
      (write-into writer o)

      (when (seq form)
        (loop [form form index 0]
          (if (= index *print-length*)
            (write-into writer "...")
            (let [f (first form)
                  n (next form)]
              (-print f writer (update opts :level inc))
              (when-not (empty? n)
                (write-into writer " ")
                (recur n (inc index)))))))

      (write-into writer (close-delim form)))))

(defn ^:private -print-seq
  [this writer opts]
  (if-some [reader-macro (reader-macros (first this))]
    (do
      (write-into writer ^String reader-macro)
      (write-into writer (pr-str (second this))))
    (-print-coll this writer opts)))

(extend-protocol Printable
  nil
  (-print [_ writer _]
    (write-into writer "nil"))

  clojure.lang.AMapEntry
  (-print [this writer opts]
    (-print-coll this writer opts))

  clojure.lang.ISeq
  (-print [this writer opts]
    (-print-seq this writer opts))

  clojure.lang.IPersistentMap
  (-print [this writer opts]
    (-print-map this writer opts))

  clojure.lang.IPersistentVector
  (-print [this writer opts]
    (-print-coll this writer opts))

  clojure.lang.IPersistentSet
  (-print [this writer opts]
    (-print-coll this writer opts))

  Object
  (-print [this writer opts]
    (if (array? this)
      (-print-seq this writer opts)
      (print-method this writer))))

(defn ^:private with-str-writer
  "Given a function, create a java.io.StringWriter (Clojure) or a
  goog.string.StringBuffer (ClojureScript), pass it to the function, and
  return the string value in the writer/buffer."
  [f]
  (with-open [writer (java.io.StringWriter.)]
    (f writer)
    (str writer)))

(defn ^:private print-linear
  "Print a form in linear style (without regard to line length).

  Given one arg (a form), print the form into a string using the
  default options.

  Given two args (a form and an options map), print the form into a
  string using the given options.

  Given three args (a java.io.Writer, a form, and an options map), print
  the form into the writer using the given options.

  Options:

    :level (long)
      The current nesting level."
  ([form]
   (print-linear form nil))
  (^String [form opts]
   (with-str-writer (fn [writer] (print-linear writer form opts))))
  ([writer form opts]
   (-print form writer opts)))

(defn ^:private print-mode
  "Given a CountKeepingWriter, a form, and an options map, return a keyword
  indicating a printing mode (:linear or :miser)."
  [writer form opts]
  (let [reserve-chars (:reserve-chars opts)
        s (print-linear form opts)]
    ;; If, after (possibly) reserving space for any closing delimiters of
    ;; ancestor S-expressions, there's enough space to print the entire
    ;; form in linear style on this line, do so.
    ;;
    ;; Otherwise, print the form in miser style.
    (if (<= (strlen s) (unchecked-subtract-int (remaining writer) reserve-chars))
      :linear
      :miser)))

(defn ^:private write-sep
  "Given a CountKeepingWriter and a printing mode, print a separator (a
  space or a newline) into the writer."
  [writer mode]
  (case mode
    :miser (nl writer)
    (write writer " ")))

(defprotocol ^:private PrettyPrintable
  (^:private -pprint [this writer opts]
    "Given a form, a CountKeepingWriter, and an options map,
      pretty-print the form into the writer.

      Options:

        :level (long)
          The current nesting level. For example, in [[:a 1]], the outer
          vector is nested at level 0, and the inner vector is nested at
          level 1.

        :indentation (String)
          A string (of spaces) to use for indentation.

        :reserve-chars (long)
          The number of characters reserved for closing delimiters of
          S-expressions above the current nesting level."))

(defn ^:private pprint-meta
  [form writer opts mode]
  (when (and *print-meta* *print-readably*)
    (when-some [m (meta form)]
      (when (seq m)
        (write writer "^")
        ;; As per https://github.com/clojure/clojure/blob/6975553804b0f8da9e196e6fb97838ea4e153564/src/clj/clojure/core_print.clj#L78-L80
        (let [m (if (and (= (count m) 1) (:tag m)) (:tag m) m)]
          (-pprint m writer opts))
        (write-sep writer mode)))))

(defn ^:private pprint-opts
  [open-delim opts]
  (let [;; The indentation level is the indentation level of the
        ;; parent S-expression plus a number of spaces equal to the
        ;; length of the open delimiter (e.g. one for "(", two for
        ;; "#{").
        padding (apply str (repeat (strlen open-delim)  " "))
        indentation (str (:indentation opts) padding)]
    (-> opts (assoc :indentation indentation) (update :level inc))))

(defn ^:private -pprint-coll
  "Like -pprint, but only for lists, vectors and sets."
  [this writer opts]
  (if (meets-print-level? (:level opts))
    (write writer "#")
    (let [[^String o form] (open-delim+form this)
          mode (print-mode writer this opts)
          opts (pprint-opts o opts)]

      ;; Print possible meta
      (pprint-meta form writer opts mode)

      ;; Print open delimiter
      (write writer o)

      ;; Print S-expression content
      (if (= *print-length* 0)
        (write writer "...")
        (when (seq form)
          (loop [form form index 0]
            (if (= index *print-length*)
              (do
                (when (= mode :miser) (write writer (:indentation opts)))
                (write writer "..."))

              (do
                ;; In miser mode, prepend indentation to every form
                ;; except the first one. We don't want to prepend
                ;; indentation for the first form, because it
                ;; immediately follows the open delimiter.
                (when (and (= mode :miser) (pos? index))
                  (write writer (:indentation opts)))

                (let [f (first form)
                      n (next form)]
                  (if (empty? n)
                    ;; This is the last child, so reserve an additional
                    ;; slot for the closing delimiter of the parent
                    ;; S-expression.
                    (-pprint f writer (update opts :reserve-chars inc))
                    (do
                      (-pprint f writer (assoc opts :reserve-chars 0))
                      (write-sep writer mode)
                      (recur n (inc index))))))))))

      ;; Print close delimiter
      (write writer (close-delim form)))))

(defn ^:private -pprint-map-entry
  "Pretty-print a map entry within a map."
  [this writer opts]
  (if (meets-print-level? (:level opts))
    (write writer "#")
    (let [k (key this)
          opts (update opts :level inc)]
      (-pprint k writer opts)

      (let [v (val this)
            ;; If, after writing the map entry key, there's enough space to
            ;; write the val on the same line, do so. Otherwise, write
            ;; indentation followed by val on the following line.
            mode (print-mode writer v (update opts :reserve-chars inc))]
        (write-sep writer mode)
        (when (= :miser mode) (write writer (:indentation opts)))
        (-pprint v writer opts)))))

(defn ^:private -pprint-map
  "Like -pprint, but only for maps."
  [this writer opts]
  (if (meets-print-level? (:level opts))
    (write writer "#")
    (let [[^String o form] (open-delim+form this)
          mode (print-mode writer this opts)
          opts (pprint-opts o opts)]
      (pprint-meta form writer opts mode)
      (write writer o)
      (if (= *print-length* 0)
        (write writer "...")
        (when (seq form)
          (loop [form form index 0]
            (if (= index *print-length*)
              (do
                (when (= mode :miser) (write writer (:indentation opts)))
                (write writer "..."))

              (do
                (when (and (= mode :miser) (pos? index))
                  (write writer (:indentation opts)))

                (let [f (first form)
                      n (next form)]
                  (if (empty? n)
                    (-pprint-map-entry f writer (update opts :reserve-chars inc))
                    (let [^String map-entry-separator (:map-entry-separator opts)]
                      ;; Reserve a slot for the map entry separator.
                      (-pprint-map-entry f writer (assoc opts :reserve-chars (strlen map-entry-separator)))
                      (write writer map-entry-separator)
                      (write-sep writer mode)
                      (recur n (inc index))))))))))

      (write writer (close-delim form)))))

(defn ^:private -pprint-seq
  [this writer opts]
  (if-some [reader-macro (reader-macros (first this))]
    (if (meets-print-level? (:level opts))
      (write writer "#")
      (do
        (write writer reader-macro)
        (-pprint (second this) writer
                 (update opts :indentation
                         (fn [indentation] (str indentation " "))))))
    (-pprint-coll this writer opts)))

(defn ^:private -pprint-queue
  [this writer opts]
  (write writer "<-")
  (-pprint-coll
   (or (seq this) '()) writer
   (update opts :indentation #(str "  " %)))
  (write writer "-<"))

(extend-protocol PrettyPrintable
  nil
  (-pprint [_ writer _]
    (write writer "nil"))

  clojure.lang.AMapEntry
  (-pprint [this writer opts]
    (-pprint-coll this writer opts))

  clojure.lang.ISeq
  (-pprint [this writer opts]
    (-pprint-seq this writer opts))

  clojure.lang.IPersistentMap
  (-pprint [this writer opts]
    (-pprint-map this writer opts))

  clojure.lang.IPersistentVector
  (-pprint [this writer opts]
    (-pprint-coll this writer opts))

  clojure.lang.IPersistentSet
  (-pprint [this writer opts]
    (-pprint-coll this writer opts))

  clojure.lang.PersistentQueue
  (-pprint [this writer opts]
    (-pprint-queue this writer opts))

  Object
  (-pprint [this writer opts]
    (if (array? this)
      (-pprint-seq this writer opts)
      (write writer (print-linear this opts)))))

(defn pprint
  "Pretty-print an object.

  Given one arg (an object), pretty-print the object into *out* using
  the default options.

  Given two args (an object and an options map), pretty-print the object
  into *out* using the given options.

  Given three args (a java.io.Writer, a object, and an options map),
  pretty-print the object into the writer using the given options.

  If *print-dup* is true, pprint does not attempt to pretty-print;
  instead, it falls back to default print-dup behavior. ClojureScript
  does not support *print-dup*.

  Options:

    :max-width (long or ##Inf, default: 72)
      Avoid printing anything beyond the column indicated by this
      value.

    :map-entry-separator (string, default: \",\")
      The string to print between map entries. To not print commas
      between map entries, use an empty string."
  ([x]
   (pprint *out* x nil))
  ([x opts]
   (pprint *out* x opts))
  ([writer x {:keys [indentation max-width map-entry-separator]
              :or {indentation "", max-width 72, map-entry-separator ","}
              :as opts}]
   (assert (or (nat-int? max-width) (= max-width ##Inf))
           ":max-width must be a natural int or ##Inf")

   (letfn
    [(pp [writer]
          ;; Allowing ##Inf was a mistake, because it's a double.
          ;;
          ;; If the user passes ##Inf, convert it to Integer/MAX_VALUE, which is
          ;; functionally the same in this case.
       (let [max-width (case max-width
                         ##Inf Integer/MAX_VALUE
                         max-width)
             writer (count-keeping-writer writer {:max-width max-width})]
         (-pprint x writer
                  (assoc opts
                         :map-entry-separator map-entry-separator
                         :level 0
                         :indentation indentation
                         :reserve-chars 0))
         (nl writer)))]
     (do
       (assert (instance? java.io.Writer writer)
               "first arg to pprint must be a java.io.Writer")

       (if *print-dup*
         (do
           (print-dup x writer)
           (.write ^java.io.Writer writer "\n"))
         (pp writer))

       (when *flush-on-newline* (.flush ^java.io.Writer writer))))))
