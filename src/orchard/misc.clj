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

(defn url?
  "Check whether the argument is an url"
  [u]
  (instance? java.net.URL u))

(defn directory?
  "Whether the argument is a directory or an url that points to a directory"
  [f]
  (if (url? f)
    (and (= (.getProtocol ^java.net.URL f) "file")
         (.isDirectory (io/as-file f)))
    (.isDirectory (io/as-file f))))

(defn file-ext?
  "Whether the argument's path ends in one of the specified case-insensitive
  file extensions"
  [f & exts]
  (when-let [file (if (url? f)
                    (when  (= (.getProtocol ^java.net.URL f) "file")
                      (io/as-file f))
                    (io/as-file f))]
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

(defn parse-java-version
  "Parse a Java version string according to JEP 223 and return the appropriate version."
  [java-ver]
  (try
    ;; the no-opt split is because a java version string can end with
    ;; an optional string consisting of a hyphen followed by other characters
    (let [[no-opt _] (str/split java-ver #"-")
          [major minor _] (str/split no-opt #"\.")
          major (Integer/parseInt major)]
      (if (> major 1)
        major
        (Integer/parseInt (or minor "7"))))
    (catch Exception _ 7)))

(def java-api-version
  (parse-java-version (System/getProperty "java.version")))

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
  {:added "0.5.0"}
  [sym]
  (when-let [ns (some-> sym namespace symbol)]
    (when-not (find-ns ns)
      (try
        (require ns)
        (catch Exception _ nil)))
    (some-> sym find-var var-get)))
