(ns orchard.info
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.java.javadoc :as javadoc]
   [orchard.classloader :refer [class-loader]]
   [orchard.java :as java]
   [orchard.meta :as m]
   [orchard.misc :as u]))

(def see-also-data
  (edn/read-string (slurp (io/resource "see-also.edn"))))

(defn see-also
  [ns sym]
  (let [var-key (str ns "/" sym)]
    (->> (get see-also-data var-key)
         (filter (comp resolve u/as-sym)))))

(defn info
  [ns sym]
  (or
   ;; it's a special (special-symbol?)
   (m/special-sym-meta sym)
   ;; it's a var
   (m/var-meta (m/resolve-var ns sym))
   ;; sym is an alias for another ns
   (m/ns-meta (get (m/resolve-aliases ns) sym))
   ;; it's simply a full ns
   (m/ns-meta (find-ns sym))
   ;; it's a Java class/member symbol...or nil
   (java/resolve-symbol ns sym)))

(defn info-java
  [class member]
  (java/member-info class member))

(defn- resource-full-path [relative-path]
  (io/resource relative-path (class-loader)))

(defn resource-path
  "If it's a resource, return a tuple of the relative path and the full resource path."
  [x]
  (or (if-let [full (resource-full-path x)]
        [x full])
      (if-let [[_ relative] (re-find #".*jar!/(.*)" x)]
        (if-let [full (resource-full-path relative)]
          [relative full]))
      ;; handles load-file on jar resources from a cider buffer
      (if-let [[_ relative] (re-find #".*jar:(.*)" x)]
        (if-let [full (resource-full-path relative)]
          [relative full]))))

(defn file-path
  "For a file path, return a URL to the file if it exists and does not
  represent a form evaluated at the REPL."
  [x]
  (when (seq x)
    (let [f (io/file x)]
      (when (and (.exists f)
                 (not (-> f .getName (.startsWith "form-init"))))
        (io/as-url f)))))

(defn file-info
  [path]
  (let [[resource-relative resource-full] (resource-path path)]
    (merge {:file (or (file-path path) resource-full path)}
           ;; Classpath-relative path if possible
           (if resource-relative
             {:resource resource-relative}))))

(defn javadoc-info
  "Resolve a relative javadoc path to a URL and return as a map. Prefer javadoc
  resources on the classpath; then use online javadoc content for core API
  classes. If no source is available, return the relative path as is."
  [^String path]
  {:javadoc
   (or (resource-full-path path)
       ;; [bug#308] `*remote-javadocs*` is outdated WRT Java
       ;; 8, so we try our own thing first.
       (when (re-find #"^(java|javax|org.omg|org.w3c.dom|org.xml.sax)/" path)
         (format "http://docs.oracle.com/javase/%d/docs/api/%s"
                 u/java-api-version path))
       ;; If that didn't work, _then_ we fallback on `*remote-javadocs*`.
       (some (let [classname (.replaceAll path "/" ".")]
               (fn [[prefix url]]
                 (when (.startsWith classname prefix)
                   (str url path))))
             @javadoc/*remote-javadocs*)
       path)})

;; TODO: Seems those were hardcoded here accidentally - we should
;; probably provide a simple API to register remote JavaDocs.
(javadoc/add-remote-javadoc "com.amazonaws." "http://docs.aws.amazon.com/AWSJavaSDK/latest/javadoc/")
(javadoc/add-remote-javadoc "org.apache.kafka." "https://kafka.apache.org/090/javadoc/index.html?")
