(ns orchard.misc
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn os-windows? []
  (.startsWith (System/getProperty "os.name") "Windows"))

(defn boot-fake-classpath
  "Retrieve Boot's fake classpath.
  When using Boot, fake.class.path contains the original directories with source
  files, which makes it way more useful than the real classpath.
  See https://github.com/boot-clj/boot/issues/249 for details."
  []
  (System/getProperty "fake.class.path"))

(defn boot-project?
  "Check whether we're dealing with a Boot project.
  We figure this by checking for the presence of Boot's fake classpath."
  []
  (not (nil? (boot-fake-classpath))))

(defn directory?
  "Whether the argument is a directory"
  [f]
  (.isDirectory (io/as-file f)))

(defn file-ext?
  "Whether the argument's path ends in one of the specified case-insensitive
  file extensions"
  [f & exts]
  (let [file (io/as-file f)]
    (some (fn [ext]
            (.endsWith (.. file getName toLowerCase) ext))
          exts)))

(defn clj-file?  [f] (file-ext? f ".clj" ".cljc"))
(defn java-file? [f] (file-ext? f ".java"))
(defn jar-file?  [f] (file-ext? f ".jar"))
(defn archive?   [f] (file-ext? f ".jar" ".zip"))

(defn as-sym
  [x]
  (cond
    (symbol? x) x
    (string? x) (if-let [[_ ns sym] (re-matches #"(.+)/(.+)" x)]
                  (symbol ns sym)
                  (symbol x))))

(defn namespace-sym
  "Return the namespace of a fully qualified symbol if possible.

  It leaves the symbol untouched if not."
  [sym]
  (if-let [ns (and sym (namespace sym))]
    (as-sym ns)
    sym))

(defn name-sym
  "Return the name of a fully qualified symbol if possible.

  It leaves the symbol untouched if not."
  [sym]
  (if-let [n (and sym (name sym))]
    (as-sym n)
    sym))

(defn update-vals
  "Update the values of map `m` via the function `f`."
  [f m]
  (reduce (fn [acc [k v]]
            (assoc acc k (f v)))
          {} m))

(defn update-keys
  "Update the keys of map `m` via the function `f`."
  [f m]
  (reduce (fn [acc [k v]]
            (assoc acc (f k) v))
          {} m))

(defn deep-merge
  "Merge maps recursively. When vals are not maps, last value wins."
  [& xs]
  (let [f (fn f [& xs]
            (if (every? map? xs)
              (apply merge-with f xs)
              (last xs)))]
    (apply f (filter identity xs))))

(def java-api-version
  (try
    (let [java-ver (System/getProperty "java.version")
          [major minor _] (str/split java-ver #"\.")
          major (Integer/parseInt major)
          minor (Integer/parseInt minor)]
      (if (> major 1)
        major
        (or minor 7)))
    (catch Exception _ 7)))

(defmulti transform-value "Transform a value for output" type)

(defmethod transform-value :default [v] (str v))

(defmethod transform-value Number [v] v)

(defmethod transform-value nil [v] nil)

(defmethod transform-value java.io.File
  [v]
  (.getAbsolutePath ^java.io.File v))

(defmethod transform-value clojure.lang.Sequential
  [v]
  (list* (map transform-value v)))

(defmethod transform-value clojure.lang.Symbol
  [v]
  (let [[the-ns the-name] [(namespace v) (name v)]]
    (if the-ns
      (str the-ns "/" the-name)
      the-name)))

(defmethod transform-value clojure.lang.Keyword
  [v]
  (transform-value (.sym ^clojure.lang.Keyword v)))

(defmethod transform-value clojure.lang.Associative
  [m]
  (->> (for [[k v] m] ; bencode keys must be strings
         [(str (transform-value k)) (transform-value v)])
       (into {})))

;; handles vectors
(prefer-method transform-value clojure.lang.Sequential clojure.lang.Associative)

;; TODO move back to analysis.cljs
(defn add-ns-macros
  "Append $macros to the input symbol"
  [sym]
  (some-> sym
          (str "$macros")
          symbol))

;; TODO move back to analysis.cljs
(defn remove-macros
  "Remove $macros from the input symbol"
  [sym]
  (some-> sym
          str
          (str/replace #"\$macros" "")
          symbol))

(defn ns-obj?
  "Return true if n is a namespace object"
  [ns]
  (instance? clojure.lang.Namespace ns))

;; Drop this in favor of clojure.core/requiring-resolve at some point?

(defn require-and-resolve
  "Try to require the namespace and get a var for the symbol, return the
  var if successful, nil if not."
  {:added "0.6.0"}
  [sym]
  (when-let [ns (some-> sym namespace symbol)]
    (when-not (find-ns ns)
      (try
        (require ns)
        (catch Exception _ nil)))
    (some-> sym find-var var-get)))
