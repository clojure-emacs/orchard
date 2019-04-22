(ns orchard.misc
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def ^:const windows-prefix
  "Windows")

(defn os-windows? []
  (.startsWith (System/getProperty "os.name") windows-prefix))

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
