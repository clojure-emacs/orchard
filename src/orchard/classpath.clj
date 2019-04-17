(ns orchard.classpath
  "Classpath access and modification"
  (:require
   [clojure.java.io :as io]
   [dynapath.util :as dp])
  (:import
   (clojure.lang Compiler DynamicClassLoader)
   (java.net URL)))

;;; Classloaders

(defn context-classloader
  "Returns the current classloader for the current thread"
  []
  (.getContextClassLoader (Thread/currentThread)))

(defn classloaders
  "Returns the thread's classloader hierarchy"
  ([^ClassLoader loader]
   (->> loader
        (iterate #(.getParent ^ClassLoader %))
        (take-while identity)))
  ([]
   (classloaders (context-classloader))))

(defn modifiable-classloader
  "Returns the highest classloader in the hierarchy that satisfies
  `dynapath.util/addable-classpath?`, or nil if none do"
  []
  (last (filter dp/addable-classpath? 
                (classloaders))))

(defn add-classloader!
  "Sets the context classloader for this thread to a
  `clojure.lang.DynamicClassLoader` using the compiler's classloader
   if available"
  []
  (let [thread (Thread/currentThread)
        loader (or @Compiler/LOADER
                   (DynamicClassLoader. (context-classloader)))]
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
                   (add-classloader!))]
    (when (dp/add-classpath-url loader url)
      url)))
