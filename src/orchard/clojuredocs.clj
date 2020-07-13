(ns orchard.clojuredocs
  "Find docs from ClojureDocs and retrieve the result as a map."
  {:author "Masashi Iizuka"
   :added "0.5"}
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [orchard.util.os :as os])
  (:import
   (java.io IOException)
   (java.net URL)
   (javax.net.ssl HttpsURLConnection)))

(def cache (atom {}))
(def default-edn-file-url
  "https://clojuredocs-edn.netlify.com/export.compact.edn")
(def cache-file-name
  (str/join os/file-separator [(os/cache-dir)
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
    (let [url (URL. url)
          conn ^HttpsURLConnection (.openConnection url)]
      (.setConnectTimeout conn connect-timeout)
      (try
        (.connect conn)
        [true]
        (catch IOException ex
          [false ex])
        (finally
          (.disconnect conn))))))

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
  If `export-edn-url` is omitted, `default-edn-file-url` is used."
  {:added "0.5"}
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
  {:added "0.5"}
  ([ns sym]
   (find-doc ns sym default-edn-file-url))
  ([ns sym export-edn-url]
   (get-doc (keyword ns sym) export-edn-url)))

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
  ([ns sym]
   (resolve-and-find-doc ns sym default-edn-file-url))
  ([ns sym export-edn-url]
   (if (special-symbol? sym)
     (find-doc "clojure.core" (str sym) export-edn-url)
     (some-> (try-ns-resolve ns sym) var-name (get-doc export-edn-url)))))

(defn- kw-to-sym [kw]
  (symbol (subs (str kw) 1)))

(defn see-also
  "Get the see-alsos for `var-name` if any."
  {:added "0.5"}
  [var-name]
  (if-let [see-alsos (:see-alsos (get-doc var-name))]
    (map kw-to-sym see-alsos)))
