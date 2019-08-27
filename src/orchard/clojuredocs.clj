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
  "Load exported documents file from ClojureDocs, and store it as a cache.
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
  "Clean a cached file and documents"
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
  "Find a document matching `ns` and `sym` from cached documents.
  Cache will be updated when there are no cached documents or cached documents are old.

  If `export-edn-url` is omitted, `default-edn-file-url` is used.

  Return nil if there is no matching document."
  {:added "0.5.0"}
  ([ns sym]
   (find-doc ns sym default-edn-file-url))
  ([ns sym export-edn-url]
   (get-doc (keyword ns sym) export-edn-url)))
