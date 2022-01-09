(def jdk8? (->> "java.version" System/getProperty (re-find #"^1.8.")))

(defproject cider/orchard "0.9.0"
  :description "A fertile ground for Clojure tooling"
  :url "https://github.com/clojure-emacs/orchard"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :scm {:name "git" :url "https://github.com/clojure-emacs/orchard"}

  :exclusions [org.clojure/clojure ; see versions matrix below
               org.clojure/clojurescript]

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

  :jvm-opts ["-Dclojure.main.report=stderr"]

  :source-paths ["src" "src-jdk8" "src-newer-jdks"]
  :test-paths ~(cond-> ["test"]
                 (not jdk8?)
                 (conj "test-newer-jdks"))

  :profiles {
             ;; Clojure versions matrix
             :provided {:dependencies [[org.clojure/clojure "1.10.3"]
                                       [org.clojure/clojure "1.10.3" :classifier "sources"]
                                       [org.clojure/clojurescript "1.11.4"]]
                        :test-paths ["test-cljs"]}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]
                                  [org.clojure/clojure "1.8.0" :classifier "sources"]]}
             :1.9 {:dependencies [[org.clojure/clojure "1.9.0"]
                                  [org.clojure/clojure "1.9.0" :classifier "sources"]]}
             :1.10 {:dependencies [[org.clojure/clojure "1.10.3"]
                                   [org.clojure/clojure "1.10.3" :classifier "sources"]]}
             :master {:repositories [["snapshots"
                                      "https://oss.sonatype.org/content/repositories/snapshots"]]
                      :dependencies [[org.clojure/clojure "1.11.0-master-SNAPSHOT"]
                                     [org.clojure/clojure "1.11.0-master-SNAPSHOT" :classifier "sources"]]}

             :test {:dependencies [[org.clojure/java.classpath "1.0.0"]]
                    :resource-paths ["test-resources"
                                     "not-a.jar"
                                     "does-not-exist.jar"]
                    :java-source-paths ["test-java"]
                    ;; Initialize the cache verbosely, as usual, so that possible issues can be more easily diagnosed:
                    :jvm-opts ["-Dorchard.initialize-cache.silent=false"
                               "-Dorchard.internal.test-suite-running=true"
                               "-Dorchard.internal.has-enriched-classpath=false"]}

             :enrich-classpath {:plugins [[mx.cider/enrich-classpath "1.5.0"]]
                                :middleware [cider.enrich-classpath/middleware]
                                :jvm-opts ["-Dorchard.internal.has-enriched-classpath=true"]}

             ;; Development tools
             :dev {:dependencies [[org.clojure/tools.namespace "1.1.0"]]
                   :source-paths ["dev"]
                   :resource-paths ["test-resources"]}

             :cljfmt {:plugins [[lein-cljfmt "0.8.0"]]
                      :cljfmt {:indents {as-> [[:inner 0]]
                                         with-debug-bindings [[:inner 0]]
                                         merge-meta [[:inner 0]]
                                         letfn [[:block 1] [:inner 2]]}}}

             :clj-kondo [:test
                         {:dependencies [[clj-kondo "2021.12.19"]]}]

             :eastwood  {:plugins  [[jonase/eastwood "1.1.0"]]
                         :eastwood {:exclude-namespaces ~(cond-> []
                                                           jdk8?
                                                           (conj 'orchard.java.parser)

                                                           (or (not jdk8?)
                                                               (not (-> "TEST_PROFILES"
                                                                        System/getenv
                                                                        (doto assert)
                                                                        (.contains "enrich-classpath"))))
                                                           (conj 'orchard.java.legacy-parser))}}})
