(ns orchard.clojuredocs
  "Find docs from ClojureDocs and retrieve the result as a map."
  {:author "Masashi Iizuka"
   :added "0.5.0"}
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [orchard.util.os :as os])
  (:import
   (java.time Instant)))

(def cache (atom {}))
(def default-edn-file-url
  "https://clojuredocs-edn.netlify.com/export.compact.edn")
(def cache-file-name
  (str/join os/file-separator [(os/cache-dir)
                               "orchard"
                               "clojuredocs"
                               "export.edn"]))
(def ^{:doc "One week. Unit is millisecond."}
  cache-updating-threshold 604800000)

(defn- write-cache-file! [url]
  (.. (io/file cache-file-name)
      getParentFile
      mkdirs)
  (->> url slurp (spit cache-file-name)))

(defn- load-cache-file! [cache-file]
  (->> cache-file
       slurp
       edn/read-string
       (reset! cache))
  true)

(defn load-cache!
  "Load exported docs file from ClojureDocs, and store it as a cache.
  A EDN format file is expected to the `export-edn-url` argument.

  If `export-edn-url` is omitted, `default-edn-file-url` is used.

  The loaded EDN file will be cached in `cache-file-name`.
  If the cached file is older than `cache-updating-threshold`,
  the cached file will be updated automatically."
  {:added "0.5.0"}
  ([]
   (load-cache! default-edn-file-url))
  ([export-edn-url]
   (let [cache-file (io/file cache-file-name)
         now-milli (-> (Instant/now) .getEpochSecond (* 1000))]
     (cond
       (or (not (.exists cache-file))
           (>= (- now-milli (.lastModified cache-file))
               cache-updating-threshold))
       (do (write-cache-file! export-edn-url)
           (load-cache-file! cache-file))

       (empty? @cache)
       (load-cache-file! cache-file)))))

(defn clean-cache!
  "Clean the cached ClojureDocs export file and the in memory cache."
  {:added "0.5.0"}
  []
  (.delete (io/file cache-file-name))
  (reset! cache {}))

(defn get-doc
  "Get data for `var-name`.
  If `export-edn-url` is omitted, `default-edn-file-url` is used."
  {:added "0.5.0"}
  ([var-name]
   (get-doc var-name default-edn-file-url))
  ([var-name export-edn-url]
   (load-cache! export-edn-url)
   (get @cache (keyword var-name))))

(defn find-doc
  "Find documentation matching `ns` and `sym` from the cached documentation.
  Cache will be updated when there is no cached documentation or when the cached documentation is old.

  If `export-edn-url` is omitted, `default-edn-file-url` is used.

  Return nil if there is no matching documentation."
  {:added "0.5.0"}
  ([ns sym]
   (find-doc ns sym default-edn-file-url))
  ([ns sym export-edn-url]
   (get-doc (keyword ns sym) export-edn-url)))

(defn- var-name
  "Convert `v`'s name to a string we can use with `get-doc`."
  [v]
  (subs (str v) 2))

(defn resolve-and-find-doc
  "Resolve `sym` in the context of `ns` and look up the documentation
  for the resulting var."
  {:added "0.5.0"}
  ([ns sym]
   (resolve-and-find-doc ns sym default-edn-file-url))
  ([ns sym export-edn-url]
   (if (special-symbol? sym)
     (find-doc "clojure.core" (str sym) export-edn-url)
     (-> (ns-resolve ns sym) var-name get-doc))))

(defn- kw-to-sym [kw]
  (symbol (subs (str kw) 1)))

(defn see-also
  "Get the see-alsos for `var-name` if any."
  {:added "0.5.0"}
  [var-name]
  (if-let [see-alsos (:see-alsos (get-doc var-name))]
    (map kw-to-sym see-alsos)))