(ns orchard.java.classpath
  "Classpath access and modification"
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [dynapath.util :as dp]
   [orchard.misc :as u])
  (:import
   (java.io File)
   (java.net URI URL)
   (java.nio.file Paths)
   (java.util.jar JarFile JarEntry)))

;;; Classloaders

(defn context-classloader
  "Returns the current classloader for the current thread"
  []
  (.getContextClassLoader (Thread/currentThread)))

(defn classloaders
  "Returns the classloader hierarchy"
  ([^ClassLoader loader]
   (->> loader
        (iterate #(.getParent ^ClassLoader %))
        (take-while identity)))
  ([]
   (classloaders (context-classloader))))

(defn modifiable-classloader
  "Returns the highest classloader in the hierarchy that satisfies
  `dynapath.util/addable-classpath?`, or nil if none do"
  ([^ClassLoader loader]
   (last (filter dp/addable-classpath?
                 (classloaders loader))))
  ([]
   (modifiable-classloader (context-classloader))))

(defn set-classloader!
  "Sets the current classloader for the current thread"
  [^ClassLoader loader]
  (let [thread (Thread/currentThread)]
    (.setContextClassLoader thread loader)
    loader))

;;; Classpaths

(defn system-classpath
  "Returns the URLs defined by the 'java.class.path' system property"
  []
  (map (comp io/as-url io/as-file)
       (.split (System/getProperty "java.class.path")
               (System/getProperty "path.separator"))))

(defn classpath
  "Returns the URLs on the classpath"
  ([^ClassLoader loader]
   (->> (classloaders loader)
        (mapcat dp/classpath-urls)
        (concat (system-classpath))
        (distinct)))
  ([]
   (classpath (context-classloader))))

(defn add-classpath!
  "Adds the URL to the classpath and returns it if successful, or nil otherwise,
  ensuring that a modifiable classloader is available"
  [^URL url]
  (let [loader (or (modifiable-classloader)
                   (set-classloader!
                    (modifiable-classloader @Compiler/LOADER)))]
    (when (dp/add-classpath-url loader url)
      url)))

;;; Classpath resources

(defn classpath-seq
  "Returns a sequence of all descendant non-directory files or archive entries
  as relative paths"
  [^URL url]
  (let [f (io/as-file url)]
    (if (u/archive? url)
      (->> (enumeration-seq (.entries (JarFile. f)))
           (filter #(not (.isDirectory ^JarEntry %)))
           (map #(.getName ^JarEntry %)))
      (->> (file-seq f)
           (filter #(not (.isDirectory ^File %)))
           (map #(.getPath (.relativize (.toURI url) (.toURI ^File %))))))))

;;; Boot's hack - previously part of cider-nrepl

(defn boot-classloader
  "Creates a class-loader that knows original source files paths in Boot project."
  []
  (let [class-path (System/getProperty "fake.class.path")
        dir-separator (System/getProperty "file.separator")
        paths (str/split class-path (re-pattern (System/getProperty "path.separator")))
        urls (map
              (fn [path]
                (let [url (if (re-find #".jar$" path)
                            (str "file:" path)
                            (str "file:" path dir-separator))]
                  (new java.net.URL url)))
              paths)]
    ;; TODO: Figure out how to add the JDK sources here
    (new java.net.URLClassLoader (into-array java.net.URL urls))))

(defn boot-aware-classloader
  []
  (if (u/boot-project?)
    (boot-classloader)
    (context-classloader)))

(defn classpath-file-relative-path
  "Boot stores files in a temporary directory & ClojureScript stores
  the :file metadata location absolutely instead of relatively to the
  classpath. This means when doing jump to source in Boot &
  ClojureScript, you end up at the temp file.  This code attempts to
  find the classpath-relative location of the file, so that it can be
  opened correctly."
  [s]
  (let [path (Paths/get s (into-array String []))
        path-count (.getNameCount path)]
    (or (first
         (sequence
          (comp (map #(.subpath path % path-count))
                (map str)
                (filter io/resource))
          (range path-count)))
        s)))
