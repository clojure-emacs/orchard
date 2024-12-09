(ns orchard.java.source-files
  "Contains functions for discovering Java source files that are already available
  on the REPL and for downloading sources from Maven."
  {:author "Oleksandr Yakushev"
   :added "0.29"}
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [orchard.misc :as misc])
  (:import (java.io File IOException)
           (java.net URL)))

(defn- readable-file [^File f]
  (when (.canRead f) f))

(defn- jdk-find
  "Search common JDK path configurations for a specified file name and return a
  URL if found. This accommodates `java.home` being set to either the JDK root
  (JDK11+) or a JRE directory within this (JDK 8), and searches both the home
  and `lib` directories."
  [f]
  (let [home (io/file (System/getProperty "java.home"))
        parent (.getParentFile home)]
    (->> [(io/file home f)
          (io/file home "lib" f)
          (io/file parent f)
          (io/file parent "lib" f)]
         (some readable-file))))

(def ^:private jdk-sources
  "The JDK sources path. If found on the existing classpath, this is the
  corresponding classpath entry. Otherwise, the JDK directory is searched for
  the file `src.zip`."
  (delay (jdk-find "src.zip")))

(defn class->classfile-path
  "Infer a relative path to the classfile of the given `klass`."
  [^Class klass]
  (let [module (when (>= misc/java-api-version 11)
                 ((requiring-resolve 'orchard.java.modules/module-name) klass))
        classfile-name (-> (.getName klass)
                           (string/replace #"\$.*" "") ;; Drop internal class.
                           (string/replace "." "/")
                           (str ".class"))]
    (cond->> classfile-name
      module (format "%s/%s" module))))

#_(class->classfile-path Thread)
#_(class->classfile-path clojure.lang.PersistentVector)

(defn classfile-path->sourcefile-path [classfile-name]
  (string/replace classfile-name #"\.class$" ".java"))

(defn- combine-archive-url ^URL [^File archive, relative-filename]
  ;; Even though the JDK stores sources in a zip archive, we still use this
  ;; function that prefixes the URL with jar:. This is fine.
  (io/as-url (format "jar:file:%s!/%s" archive relative-filename)))

(defn- locate-source-url-on-classpath ^URL [klass]
  (-> klass
      class->classfile-path
      classfile-path->sourcefile-path
      io/resource))

#_(locate-source-url-on-classpath mx.cider.orchard.LruMap)

(defn- locate-source-url-in-jdk-sources
  "Try to find the source file for `klass` in sources included with JDK."
  ^URL [^Class klass]
  ;; Heuristic: JDK classes have `nil` classloader.
  (when (and @jdk-sources (nil? (.getClassLoader klass)))
    (let [source-file (-> klass
                          class->classfile-path
                          classfile-path->sourcefile-path)]
      ;; Even though the JDK stores sources in a zip archive, we return the
      ;; path as "jar:...". This is fine.
      (combine-archive-url @jdk-sources source-file))))

#_(locate-source-url-in-jdk-sources Thread)

(defn- parse-jar-path-from-url [^URL url]
  (when-let [[_ path] (some->> url .getFile (re-matches #"file:(.+\.jar)!.*"))]
    (readable-file (io/file path))))

(defn- infer-sources-jar-file [^File jar-file]
  (let [parent (.getParent jar-file)
        [_ fname] (re-matches #"(.+)\.jar" (.getName jar-file))
        sources-jar (io/file parent (str fname "-sources.jar"))]
    (readable-file sources-jar)))

(defn- locate-source-url-near-class-jar
  "If `klass` comes from a third-party JAR that presumedly resides in `.m2`
  directory, try to look for a sources JAR near the class JAR and return it
  together with the implied source filename."
  [^Class klass]
  (when-let [cl (some-> klass .getClassLoader)]
    ;; Get the classloader that loaded the `klass` and locate the resource that
    ;; the presumed classfile that the class was loaded from. If that resource
    ;; is a JAR file, search for a sources JAR near it.
    (let [class-file (class->classfile-path klass)
          res (.getResource cl class-file)
          sources-jar-file (some-> res parse-jar-path-from-url infer-sources-jar-file)
          source-filename (classfile-path->sourcefile-path class-file)]
      (when sources-jar-file
        (combine-archive-url sources-jar-file source-filename)))))

#_(locate-source-url-near-class-jar clojure.lang.PersistentVector)

(defn class->source-file-url ^URL [klass]
  (when-let [url (or (locate-source-url-on-classpath klass)
                     (locate-source-url-in-jdk-sources klass)
                     (locate-source-url-near-class-jar klass))]
    ;; Check if URL can be read before returning it.
    (try (.getContent ^URL url)
         url
         (catch IOException _))))

#_(class->source-file-url mx.cider.orchard.LruMap)
#_(class->source-file-url java.lang.Thread)
#_(class->source-file-url clojure.lang.PersistentVector)
#_(class->source-file-url clojure.core.Eduction)
