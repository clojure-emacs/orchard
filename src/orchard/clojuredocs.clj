(ns orchard.clojuredocs
  "Find docs from ClojureDocs and retrieve the result as a map."
  {:author "Masashi Iizuka"
   :added "0.5"}
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [orchard.util.os :as os])
  (:import
   (java.io IOException)
   (java.net URI)
   (javax.net.ssl HttpsURLConnection)))

(def cache (atom {}))
(def default-edn-file-url
  "https://github.com/clojure-emacs/clojuredocs-export-edn/raw/master/exports/export.compact.edn")
(def cache-file-name
  (string/join os/file-separator [(os/cache-dir)
                                  "orchard"
                                  "clojuredocs"
                                  "export.edn"]))

(def connect-timeout
  "Timeout value for checking connection. Unit is millisecond."
  1000)

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

(defn test-remote-url [^String url]
  (if-not (.startsWith url "http")
    ;; Skip checks for non remote url
    [true]
    (let [url (.toURL (URI. url))
          conn ^HttpsURLConnection (.openConnection url)]
      (.setConnectTimeout conn connect-timeout)
      (try
        (.connect conn)
        [true]
        (catch IOException ex
          [false ex])
        (finally
          (.disconnect conn))))))

(defn load-docs-if-not-loaded!
  "Load exported docs from bundled or cached file when no docs are loaded.
  The Cached file take priority."
  {:added "0.5"}
  []
  (when (empty? @cache)
    (let [cache-file (io/file cache-file-name)]
      (load-cache-file!
       (if (.exists cache-file)
         cache-file
         (io/resource "clojuredocs/export.edn"))))))

(defn update-cache!
  "Load exported docs file from ClojureDocs, and store it as a cache.
  A EDN format file is expected to the `export-edn-url` argument.

  If `export-edn-url` is omitted, `default-edn-file-url` is used.

  If `export-edn-url` is not accessible, `IOException` is thrown.
  If `export-edn-url` is not a URL for remote host, `IllegalArgumentException` is thrown."
  {:added "0.5"}
  ([]
   (update-cache! default-edn-file-url))
  ([export-edn-url]
   (let [cache-file (io/file cache-file-name)
         ;; connection check not to wait too long
         [downloadable? conn-ex] (test-remote-url export-edn-url)]
     (if (not downloadable?)
       (throw conn-ex)
       (do (write-cache-file! export-edn-url)
           (load-cache-file! cache-file))))))

(defn clean-cache!
  "Clean the cached ClojureDocs export file and the in memory cache."
  {:added "0.5"}
  []
  (.delete (io/file cache-file-name))
  (reset! cache {}))

(defn get-doc
  "Get data for `var-name`.
  Bundled documentation will be used when there is no cached documentation."
  {:added "0.5"}
  [var-name]
  (load-docs-if-not-loaded!)
  (get @cache (keyword var-name)))

(defn find-doc
  "Find documentation matching `ns` and `sym` from the cached documentation.
  Bundled documentation will be used when there is no cached documentation.

  Return nil if there is no matching documentation."
  {:added "0.5"}
  [ns sym]
  (get-doc (keyword ns sym)))

(defn- var-name
  "Convert `v`'s name to a string we can use with `get-doc`."
  [v]
  (subs (str v) 2))

(defn- try-ns-resolve [ns sym]
  (try
    (ns-resolve ns sym)
    (catch Exception _
      nil)))

(defn resolve-and-find-doc
  "Resolve `sym` in the context of `ns` and look up the documentation
  for the resulting var."
  {:added "0.5"}
  [ns sym]
  (if (special-symbol? sym)
    (find-doc "clojure.core" (str sym))
    (some-> (try-ns-resolve ns sym) var-name get-doc)))

(defn- kw-to-sym [kw]
  (symbol (subs (str kw) 1)))

(defn see-also
  "Get the see-alsos for `var-name` if any."
  {:added "0.5"}
  [var-name]
  (when-let [see-alsos (:see-alsos (get-doc var-name))]
    (map kw-to-sym see-alsos)))
