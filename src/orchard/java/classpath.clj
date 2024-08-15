(ns orchard.java.classpath
  "Classpath access and modification utilities.

  Provides an alternative to the java.classpath contrib library."
  (:require
   [clojure.java.io :as io]
   [orchard.misc :as misc])
  (:import
   (java.io File)
   (java.net URL URLClassLoader)
   (java.util.jar JarEntry JarFile)))

;;; Classloaders

(defn context-classloader
  "Returns the current classloader for the current thread."
  []
  (.getContextClassLoader (Thread/currentThread)))

(defn classloaders
  "Returns the classloader hierarchy."
  ([^ClassLoader loader]
   (->> loader
        (iterate #(.getParent ^ClassLoader %))
        (take-while identity)))
  ([]
   (classloaders (context-classloader))))

(defn ^:deprecated modifiable-classloader
  ([_])
  ([]))

(defn set-classloader!
  "Sets the current classloader for the current thread."
  [^ClassLoader loader]
  (let [thread (Thread/currentThread)]
    (.setContextClassLoader thread loader)
    loader))

;;; Classpaths

(defn system-classpath
  "Returns the URLs defined by the 'java.class.path' system property."
  []
  (map (comp io/as-url io/as-file)
       (.split (System/getProperty "java.class.path")
               (System/getProperty "path.separator"))))

(defn classpath-urls [classloader]
  (if-not (instance? URLClassLoader classloader)
    nil
    (-> ^URLClassLoader classloader .getURLs seq)))

(defn classpath
  "Returns the URLs on the classpath."
  ([^ClassLoader loader]
   (->> (classloaders loader)
        (mapcat classpath-urls)
        (concat (system-classpath))
        (distinct)))
  ([]
   (classpath (context-classloader))))

(defn ^:deprecated add-classpath!
  "Adds the URL to the classpath and returns it if successful, or nil otherwise,
  ensuring that a modifiable classloader is available."
  [_]
  nil)

;;; Classpath resources

(defn classpath-seq
  "Returns a sequence of all descendant non-directory files or archive entries
  as relative paths."

  ([url]
   (classpath-seq url nil))

  ([^URL url, file-seq-fn]
   (let [f (io/as-file url)]
     (cond
       (not (.exists f))
       []

       (misc/archive? url)
       (->> (enumeration-seq (.entries (JarFile. f)))
            (filter #(not (.isDirectory ^JarEntry %)))
            (map #(.getName ^JarEntry %)))

       :else
       (->> (if file-seq-fn
              (file-seq-fn f)
              (file-seq f))
            (filter #(not (.isDirectory ^File %)))
            (map #(.getPath (.relativize (.toURI url) (.toURI ^File %)))))))))
