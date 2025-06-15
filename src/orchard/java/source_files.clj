(ns orchard.java.source-files
  "Contains functions for discovering Java source files that are already available
  on the REPL and for downloading sources from Maven."
  {:author "Oleksandr Yakushev"
   :added "0.29"}
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [orchard.java.compatibility :as compat])
  (:import (java.io File IOException)
           (java.net URL)))

(def ^:dynamic *download-sources-jar-fn*
  "When not nil, this function will be called on the Class object if none of the
  resolving methods could discover the source file for that class. The bound
  function should try to download the JAR and return true if it did."
  nil)

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

(defn- class->classfile-path
  "Infer a relative path to the classfile of the given `klass`."
  [^Class klass]
  (let [module (compat/module-name klass)
        classfile-name (-> (.getName klass)
                           (str/replace #"\$.*" "") ;; Drop internal class.
                           (str/replace "." "/")
                           (str ".class"))]
    (cond->> classfile-name
      module (format "%s/%s" module))))

(defn- classfile-path->sourcefile-path [classfile-name]
  (str/replace classfile-name #"\.class$" ".java"))

(defn class->sourcefile-path
  "Infer a relative path to a source file of the given `klass`."
  [^Class klass]
  (-> klass class->classfile-path classfile-path->sourcefile-path))

#_(class->sourcefile-path Thread)
#_(class->sourcefile-path clojure.lang.PersistentVector)

(defn- verify-url-readable
  "Try to open `url` for reading. If it is readable, return `url`, otherwise nil."
  [^URL url]
  (when url
    (try (.getContent ^URL url)
         url
         (catch IOException _))))

(defn- locate-source-url-on-classpath ^URL [klass]
  (-> klass
      class->sourcefile-path
      io/resource
      verify-url-readable))

#_(locate-source-url-on-classpath mx.cider.orchard.LruMap)

(defn- combine-archive-url ^URL [^File archive, relative-filename]
  ;; Even though the JDK stores sources in a zip archive, we still use this
  ;; function that prefixes the URL with jar:. This is fine.
  (try
    (let [uri-str (str (.toURI archive))]
      (when (str/starts-with? uri-str "file:")
        (io/as-url (format "jar:%s!/%s" uri-str relative-filename))))
    ;; Exceptions might happen when creating an URL, protect users from them.
    (catch java.net.MalformedURLException _)))

(defn- locate-source-url-in-jdk-sources
  "Try to find the source file for `klass` in sources included with JDK."
  ^URL [^Class klass]
  ;; Heuristic: JDK classes have `nil` classloader.
  (when (and @jdk-sources (nil? (.getClassLoader klass)))
    (let [source-file (class->sourcefile-path klass)]
      (-> (combine-archive-url @jdk-sources source-file)
          verify-url-readable))))

#_(locate-source-url-in-jdk-sources Thread)

(defn- parse-jar-path-from-url [^URL url]
  (when-let [[_ path] (some->> url .getFile (re-matches #"file:(.+\.jar)!.*"))]
    (readable-file (io/file path))))

(defn- infer-sources-jar-file [^File jar-file]
  (let [parent (.getParent jar-file)
        [_ fname] (re-matches #"(.+)\.jar" (.getName jar-file))
        sources-jar (io/file parent (str fname "-sources.jar"))]
    (readable-file sources-jar)))

(defn- find-jar-file-for-class [^Class klass]
  ;; Get the classloader that loaded `klass` and locate the resource that the
  ;; class was loaded from. If that resource is a JAR file, return it.
  (when-let [cl (.getClassLoader klass)]
    (let [class-file (class->classfile-path klass)
          res (.getResource cl class-file)]
      (some-> res parse-jar-path-from-url))))

(defn- locate-source-url-near-class-jar
  "If `klass` comes from a third-party JAR that presumedly resides in `.m2`
  directory, try to look for a sources JAR near the class JAR and return it
  together with the implied source filename."
  [^Class klass]
  (let [class-jar-file (find-jar-file-for-class klass)
        sources-jar-file (some-> class-jar-file infer-sources-jar-file)
        class-file (class->classfile-path klass)
        source-filename (classfile-path->sourcefile-path class-file)]
    (when sources-jar-file
      (-> (combine-archive-url sources-jar-file source-filename)
          verify-url-readable))))

#_(locate-source-url-near-class-jar clojure.lang.PersistentVector)

(defn class->source-file-url
  "Resolve the URL path to the sources given `klass` using any of the supported
  methods. If no method worked and `*download-sources-jar-fn*` is bound, invoke
  it and try to resolve again."
  ^URL [klass]
  (or (locate-source-url-on-classpath klass)
      (locate-source-url-in-jdk-sources klass)
      (locate-source-url-near-class-jar klass)
      (and *download-sources-jar-fn*
           (*download-sources-jar-fn* klass)
           (locate-source-url-near-class-jar klass))))

#_(class->source-file-url mx.cider.orchard.LruMap)
#_(class->source-file-url java.lang.Thread)
#_(class->source-file-url clojure.lang.PersistentVector)
#_(class->source-file-url clojure.core.Eduction)

;;; ## Downloading Java sources from Maven

(defn infer-maven-coordinates-for-class
  "Given a class, attempt to parse its Maven coordinates (group id, artifact id,
  version) from the JAR path it resides in."
  [^Class klass]
  (when-let [^File jar-file (find-jar-file-for-class klass)]
    ;; Leap of faith here. We really infer this data from the filesystem path.
    ;; The correct way would be to parse pom.xml inside the jar and get the
    ;; coordinates from there, but that is more work.
    (let [jar-file'   (.getParentFile jar-file)
          version     (.getName jar-file')
          jar-file''  (.getParentFile jar-file')
          artifact    (.getName jar-file'')
          group-parts (loop [acc (), f jar-file'']
                        (let [parent (.getParentFile f)
                              name   (some-> parent .getName)]
                          (if (and name (not= name "repository"))
                            (recur (cons name acc) parent)
                            acc)))]
      {:artifact artifact
       :group    (str/join "." group-parts)
       :version  version})))

#_(infer-maven-coordinates-for-class clojure.lang.PersistentVector)

(defn- download-sources-using-invoke-tool
  "Download source JAR for given library coordinates using Clojure 1.12
  `invoke-tool` wrapper."
  [{:keys [group artifact version]}]
  (let [procurer (or ((requiring-resolve 'clojure.java.basis/current-basis))
                     {:mvn/repos {"central" {:url "https://repo1.maven.org/maven2/"}
                                  "clojars" {:url "https://repo.clojars.org/"}}})
        sources-artifact (symbol (format "%s/%s$sources" group artifact))
        lib-coords {sources-artifact {:mvn/version version}}
        res ((requiring-resolve 'clojure.tools.deps.interop/invoke-tool)
             {:tool-alias :deps
              :fn 'clojure.tools.deps/resolve-added-libs
              :args {:add lib-coords, :procurer procurer}})]
    (vec (mapcat :paths (vals (:added res))))))

#_(download-sources-using-invoke-tool (infer-maven-coordinates-for-class clojure.lang.PersistentVector))

(defn- run-subprocess [args]
  (println "[Orchard] Invoking subprocess:" (str/join " " args))
  (let [process (.start (ProcessBuilder. ^java.util.List args))]
    (.waitFor process)
    (let [out (slurp (.getInputStream process))
          err (slurp (.getErrorStream process))]
      (when-not (str/blank? out)
        (println out))
      (when-not (str/blank? err)
        (binding [*out* *err*] (println err))))
    (.exitValue process)))

(defn- download-sources-using-clojure-cli
  "Download source JAR for given library coordinates by invoking `clojure` CLI
  subprocess."
  [{:keys [group artifact version]}]
  (when (System/getProperty "clojure.basis")
    (run-subprocess ["clojure" "-P" "-Sdeps"
                     (format "{:deps {%s/%s$sources {:mvn/version \"%s\"}}}"
                             group artifact version)])))

#_(download-sources-using-clojure-cli (infer-maven-coordinates-for-class clojure.lang.PersistentVector))

(defn- download-sources-using-lein
  "Download source JAR for given library coordinates by invoking `lein`
  subprocess."
  [{:keys [group artifact version]}]
  (run-subprocess
   ;; "update-in :dependencies empty" is needed to drop the existing deps so
   ;; that they don't mess with the sources JAR version we need.
   ["lein" "update-in" ":dependencies" "empty" "--"
    "update-in" ":dependencies" "conj"
    (format "[%s/%s \"%s\" :classifier \"sources\"]" group artifact version)
    "--" "deps"]))

#_(download-sources-using-lein (infer-maven-coordinates-for-class clojure.lang.PersistentVector))

(defn download-sources-jar-for-coordinates
  "Download source JAR for given library coordinates using either tools.deps or
  Leiningen, depending how the Clojure process was started. Returns non-nil if
  any of the methods at least attempted to download the artifact, or nil if
  downloading didn't happen."
  [maven-coordinates]
  (let [clj-1-12+? (try (require 'clojure.tools.deps.interop)
                        (catch Exception _))
        tools-deps? (System/getProperty "clojure.basis")]
    (cond (and tools-deps? clj-1-12+?)
          (download-sources-using-invoke-tool maven-coordinates)

          tools-deps?
          (download-sources-using-clojure-cli maven-coordinates)

          (System/getenv "LEIN_HOME") ;; Indicator of Leiningen-started process.
          (download-sources-using-lein maven-coordinates))))

#_(download-sources-jar-for-coordinates (infer-maven-coordinates-for-class clojure.lang.PersistentVector))
