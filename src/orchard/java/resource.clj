(ns orchard.java.resource
  "Resolve JVM resource-related information."
  {:added "0.5"}
  (:require
   [clojure.java.io :as io]
   [orchard.java.classpath :as cp])
  (:import (java.io File)
           (java.net URL)))

(defn- trim-leading-separator
  "Trim the java.io.File/separator at the beginning of s."
  [^String s]
  (if (.startsWith s java.io.File/separator)
    (subs s 1)
    s))

(defn project-resources
  "Get a list of classpath resources."
  []
  (mapcat
   (fn [^File directory]
     (->> directory
          (file-seq)
          (filter (memfn ^File isFile))
          (map (fn [^File file]
                 (let [relpath (-> file
                                   (.getPath)
                                   (.replaceFirst
                                    (.getPath directory)
                                    "")
                                   (trim-leading-separator))]
                   {:root directory
                    :file file
                    :relpath relpath
                    :url (io/resource relpath)})))
          (remove #(.startsWith ^String (:relpath %) "META-INF/"))
          (remove #(re-matches #".*\.(clj[cs]?|java|class)" (:relpath %)))))
   (filter (memfn ^File isDirectory) (map io/as-file (cp/classpath (cp/boot-aware-classloader))))))

(defn resource-full-path ^URL [relative-path]
  (io/resource relative-path (cp/boot-aware-classloader)))

(defn resource-path-tuple
  "If it's a resource, return a tuple of the relative path and the full
  resource path."
  [path]
  (or (if-let [full (resource-full-path path)]
        [path full])
      (if-let [[_ relative] (re-find #".*jar!/(.*)" path)]
        (if-let [full (resource-full-path relative)]
          [relative full]))
      ;; handles load-file on jar resources from a cider buffer
      (if-let [[_ relative] (re-find #".*jar:(.*)" path)]
        (if-let [full (resource-full-path relative)]
          [relative full]))))

(defn resource-path
  "Return the resource path for the given name."
  {:added "0.5"}
  [name]
  (some-> name (resource-full-path) (.getPath)))

(defn resource-maps
  "Return a seq of resource maps:

    {:file    \"the absolute path to the resource\"
     :relpath \"the path of the resource relative to the classpath\"}

  If the project does not contain resources, it returns nil."
  {:added "0.5"}
  []
  (map #(select-keys % [:file :relpath])
       (project-resources)))
