;;;; The following code allows to add the JDK sources without `dynapath` being present.

(require '[clojure.java.io :as io])

(import '[java.util.zip ZipInputStream]
        '[java.io FileOutputStream])

(defmacro while-let [[sym expr] & body]
  `(loop [~sym ~expr]
     (when ~sym
       ~@body
       (recur ~expr))))

(defn jdk-find [f]
  (let [home (io/file (System/getProperty "java.home"))
        parent (.getParentFile home)
        paths [(io/file home f)
               (io/file home "lib" f)
               (io/file parent f)
               (io/file parent "lib" f)]]
    (->> paths (filter #(.canRead ^java.io.File %)) first str)))

(def jdk-sources
  (let [java-path->zip-path (fn [path]
                              (some-> (io/resource path)
                                      ^java.net.JarURLConnection (. openConnection)
                                      (. getJarFileURL)
                                      io/as-file
                                      str))]
    (or (java-path->zip-path "java.base/java/lang/Object.java") ; JDK9+
        (java-path->zip-path "java/lang/Object.java")           ; JDK8-
        (jdk-find "src.zip"))))

(defn uncompress [path target]
  (let [zis (-> target io/input-stream ZipInputStream.)]
    (while-let [entry (-> zis .getNextEntry)]
      (let [size (-> entry .getSize)
            bytes (byte-array 1024)
            dest (->> entry .getName (io/file path))
            dir (-> entry .getName (clojure.string/split #"/") butlast)
            _ (->> (clojure.string/join "/" dir) (java.io.File. path) .mkdirs)
            output (FileOutputStream. dest)]
        (loop [len (-> zis (.read bytes))]
          (when (pos? len)
            (-> output (.write bytes 0 len))
            (recur (-> zis (.read bytes)))))
        (-> output .close)))))

(defn unzipped-jdk-source []
  (let [choice jdk-sources]
    (when-not (-> "unzipped-jdk-source" io/file .exists)
      (-> "unzipped-jdk-source" io/file .mkdirs)
      ;; For some reason simply adding a .zip to the classpath doesn't work, so one has to uncompress the contents:
      (uncompress "./unzipped-jdk-source/" choice))
    "unzipped-jdk-source"))

(def jdk8? (->> "java.version" System/getProperty (re-find #"^1.8.")))

;;;; Project definition

(defproject cider/orchard "0.7.1"
  :description "A fertile ground for Clojure tooling"
  :url "https://github.com/clojure-emacs/orchard"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :scm {:name "git" :url "https://github.com/clojure-emacs/orchard"}

  :dependencies [[org.tcrawley/dynapath "1.1.0"]
                 [org.clojure/clojurescript "1.10.520"]]
  :exclusions [org.clojure/clojure] ; see versions matrix below

  :aliases {"bump-version" ["change" "version" "leiningen.release/bump-version"]}

  :release-tasks [["vcs" "assert-committed"]
                  ["bump-version" "release"]
                  ["vcs" "commit" "Release %s"]
                  ["vcs" "tag" "v" "--no-sign"]
                  ["bump-version"]
                  ["vcs" "commit" "Begin %s"]]

  :deploy-repositories [["clojars" {:url "https://clojars.org/repo"
                                    :username :env/clojars_username
                                    :password :env/clojars_password
                                    :sign-releases false}]]

  :jvm-opts ["-Dorchard.use-dynapath=true"
             "-Dclojure.main.report=stderr"]

  :profiles {
             ;; Clojure versions matrix
             :provided {:dependencies [[org.clojure/clojure "1.10.1"]
                                       [org.clojure/clojure "1.10.1" :classifier "sources"]]}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]
                                  [org.clojure/clojure "1.8.0" :classifier "sources"]]}
             :1.9 {:dependencies [[org.clojure/clojure "1.9.0"]
                                  [org.clojure/clojure "1.9.0" :classifier "sources"]]}
             :1.10 {:dependencies [[org.clojure/clojure "1.10.1"]
                                   [org.clojure/clojure "1.10.1" :classifier "sources"]]}
             :master {:repositories [["snapshots"
                                      "https://oss.sonatype.org/content/repositories/snapshots"]]
                      :dependencies [[org.clojure/clojure "1.11.0-master-SNAPSHOT"]
                                     [org.clojure/clojure "1.11.0-master-SNAPSHOT" :classifier "sources"]]}

             :test {:dependencies [[org.clojure/java.classpath "1.0.0"]]
                    :resource-paths ["test-resources"
                                     "not-a.jar"
                                     "does-not-exist.jar"]
                    ;; Initialize the cache verbosely, as usual, so that possible issues can be more easily diagnosed:
                    :jvm-opts ["-Dorchard.initialize-cache.silent=false"
                               "-Dorchard.internal.test-suite-running=true"]}

             :no-dynapath {:jvm-opts ["-Dorchard.use-dynapath=false"]
                           :resource-paths [~(unzipped-jdk-source)]
                           :plugins ~(if jdk8?
                                       '[[lein-jdk-tools "0.1.1"]]
                                       [])}

             ;; Development tools
             :dev {:dependencies [[pjstadig/humane-test-output "0.10.0"]]
                   :resource-paths ["test-resources"]
                   :plugins [[com.jakemccrary/lein-test-refresh "0.23.0"]]
                   :injections [(require 'pjstadig.humane-test-output)
                                (pjstadig.humane-test-output/activate!)]
                   :test-refresh {:changes-only true}}

             :cljfmt {:plugins [[lein-cljfmt "0.8.0"]]
                      :cljfmt {:indents {as-> [[:inner 0]]
                                         with-debug-bindings [[:inner 0]]
                                         merge-meta [[:inner 0]]
                                         letfn [[:block 1] [:inner 2]]}}}

             :clj-kondo [:test
                         {:dependencies [[clj-kondo "2021.09.15"]]}]

             :eastwood  {:plugins  [[jonase/eastwood "0.9.9"]]
                         :eastwood {:exclude-namespaces [~(if jdk8?
                                                            'orchard.java.parser
                                                            'orchard.java.legacy-parser)]}}})
