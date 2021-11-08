(ns orchard.util.io
  "Utility functions for dealing with file system objects, and in/out streams."
  (:require
   [clojure.java.io :as io])
  (:import
   (java.io File)
   (java.net URL JarURLConnection)))

(defn wrap-silently
  "Middleware that executes `(f)` without printing to `System/out` or `System/err`.

  (Note that `System/out` is different from `*out*`)"
  [f]
  (fn []
    (let [old-out System/out
          old-err System/err
          ps (java.io.PrintStream. (proxy [java.io.OutputStream] []
                                     (write
                                       ([a])
                                       ([a b c])
                                       ([a b c d e]))))]
      (try
        (System/setOut ps)
        (System/setErr ps)
        (f)
        (finally
          (when (= ps System/out) ;; `System/out` may have changed in the meantime (in face of concurrency)
            (System/setOut old-out))
          (when (= ps System/err) ;; `System/err` may have changed in the meantime (in face of concurrency)
            (System/setErr old-err)))))))

(defn url-protocol
  "Get the URL protocol as a string, e.g. http, file, jar."
  ^String
  [^java.net.URL url]
  (.getProtocol url))

(def jar? #{"jar"})

(def file? #{"file"})

(defn url-to-file-within-archive?
  "Does this URL point to a file inside a jar (or zip) archive?
  i.e., does it use the jar: protocol."
  [^java.net.URL url]
  (jar? (url-protocol url)))

(defn direct-url-to-file?
  "Does this URL point to a file directly?
  i.e., does it use the file: protocol."
  [^java.net.URL url]
  (file? (url-protocol url)))

(defn resource-jarfile
  "Given a jar:file:...!/... URL, return the location of the jar file on the
  filesystem. Returns nil on any other URL."
  ^File [^URL jar-resource]
  (assert (jar? (url-protocol jar-resource)))
  (let [^JarURLConnection conn (.openConnection jar-resource)
        inner-url (.getJarFileURL conn)]
    (when (file? (url-protocol inner-url))
      (io/as-file inner-url))))

(defn resource-artifact
  "Return the File from which the given resource URL would be loaded.

  For `file:` URLs returns the location of the resource itself, for
  `jar:..!/...` URLs returns the location of the archive containing the
  resource. Returns a fully qualified File, even when the URL is relative.
  Throws when the URL is not a `file:` or `jar:` URL."
  ^File [^java.net.URL resource]
  (let [protocol (url-protocol resource)]
    (cond
      (file? protocol)
      (io/as-file resource)

      (jar? protocol)
      (resource-jarfile resource)

      :else
      (throw (ex-info (str "URLs with a " protocol
                           " protocol can't be situated on the filesystem.")
                      {:resource resource})))))

(defprotocol LastModifiedTime
  (last-modified-time [this]
    "Return the last modified time of a File or resource URL."))

(extend-protocol LastModifiedTime
  java.net.URL
  (last-modified-time [this]
    (last-modified-time (resource-artifact this)))
  java.io.File
  (last-modified-time [this]
    (.lastModified this)))
