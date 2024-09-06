(def jdk8? (= (System/getProperty "java.specification.version") "1.8"))
(def jdk21? (= (System/getProperty "java.specification.version") "21"))

;; Needed to be added onto classpath to test Java parser functionality.
(def jdk-21-sources-archive
  (delay
    (let [src-zip (clojure.java.io/file "base-src.zip")]
      (if (.exists src-zip)
        (do (println "Found JDK sources:" src-zip)
            [src-zip])
        (do (println "base-src.zip not found. Run `make base-src.zip` to properly run all the tests.")
            nil)))))

;; Needed to run eastwood on JDK8.
(def tools-jar
  (delay
    (let [java-home (System/getProperty "java.home")
          tools-jar-paths [(clojure.java.io/file java-home "tools.jar")
                           (clojure.java.io/file java-home "lib" "tools.jar")
                           (clojure.java.io/file java-home ".." "tools.jar")
                           (clojure.java.io/file java-home ".." "lib" "tools.jar")]
          tools-jar (some #(when (.exists %) %) tools-jar-paths)]
      (assert tools-jar (str "tools.jar was not found in " java-home))
      (println "Found tools.jar:" tools-jar)
      (str tools-jar))))

(def dev-test-common-profile
  {:dependencies '[[org.clojure/java.classpath "1.1.0"]
                   [nubank/matcher-combinators "3.9.1"
                    :exclusions [org.clojure/clojure]]]
   :source-paths (cond-> ["test-java" "java"]
                   ;; We only include sources with JDK21 because we only
                   ;; repackage sources for that JDK. Sources from one JDK are
                   ;; not compatible with other JDK for our test purposes.
                   jdk21? (into @jdk-21-sources-archive))
   :resource-paths ["test-resources"]
   :test-paths ["test"]
   :java-source-paths ["test-java"]})

(defproject cider/orchard (or (not-empty (System/getenv "PROJECT_VERSION"))
                              "0.0.0")
  :description "A fertile ground for Clojure tooling"
  :url "https://github.com/clojure-emacs/orchard"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :scm {:name "git" :url "https://github.com/clojure-emacs/orchard"}

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

  :jvm-opts ["-Dclojure.main.report=stderr"]

  :source-paths ["src"]
  :test-paths ["test"]
  :java-source-paths ["java"]

  :javac-options ["-Xlint:unchecked"]

  :profiles {:provided {:dependencies [[org.clojure/clojure "1.12.0"]
                                       [org.clojure/clojure "1.12.0" :classifier "sources"]]}

             :1.10 {:dependencies [[org.clojure/clojure "1.10.3"]
                                   [org.clojure/clojure "1.10.3" :classifier "sources"]]}
             :1.11 {:dependencies [[org.clojure/clojure "1.11.4"]
                                   [org.clojure/clojure "1.11.4" :classifier "sources"]]}
             :1.12 {:dependencies [[org.clojure/clojure "1.12.0"]
                                   [org.clojure/clojure "1.12.0" :classifier "sources"]]}

             ;; Needed to test how Orchard behaves with Clojurescript on classpath.
             :cljs {:dependencies [[org.clojure/clojurescript "1.11.132"]]
                    :test-paths ["test-cljs"]}

             :spec2 {:source-paths ["submodules/spec-alpha2/src/main/clojure"]}

             :test ~(merge
                     dev-test-common-profile
                     ;; Initialize the cache verbosely, as usual, so that possible issues can be more easily diagnosed:
                     {:jvm-opts
                      ["-Dorchard.initialize-cache.silent=false"
                       "-Dorchard.internal.test-suite-running=true"]
                      :resource-paths ["test-resources"
                                       "test-java-invalid"
                                       "not-a.jar"
                                       "does-not-exist.jar"]})

             :parser-next {:jvm-opts ["--add-opens=jdk.compiler/com.sun.tools.javac.code=ALL-UNNAMED"
                                      "--add-opens=jdk.compiler/com.sun.tools.javac.tree=ALL-UNNAMED"]}

             ;; Development tools
             :dev ~(-> dev-test-common-profile
                       (update :source-paths conj "dev")
                       (update :dependencies conj '[org.clojure/tools.namespace "1.5.0"]))

             :cljfmt {:plugins [[lein-cljfmt "0.9.2"]]
                      :cljfmt {:indents {merge-meta [[:inner 0]]}}}

             :clj-kondo {:plugins [[com.github.clj-kondo/lein-clj-kondo "2023.07.13"]]}

             :eastwood  {:source-paths ~(if jdk8? [@tools-jar] [])
                         :plugins  [[jonase/eastwood "1.4.0"]]
                         :eastwood {:ignored-faults {:unused-ret-vals-in-try {orchard.java {:line 84}
                                                                              orchard.java.parser-next-test true}}
                                    :exclude-namespaces ~(if jdk8?
                                                           '[orchard.java.modules
                                                             orchard.java.parser
                                                             orchard.java.parser-test
                                                             orchard.java.parser-utils
                                                             orchard.java.parser-next
                                                             orchard.java.parser-next-test]
                                                           '[orchard.java.legacy-parser])}}

             :deploy {:source-paths [".circleci/deploy"]}})
