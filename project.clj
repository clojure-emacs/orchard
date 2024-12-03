(def jdk-version
  (let [v (System/getProperty "java.specification.version")]
    (if (.contains v ".") 8 (Integer/parseInt v))))
(def jdk8? (= jdk-version 8))

;; Needed to be added onto classpath to test Java parser functionality.
(def jdk-sources-archive
  (delay
    (when (>= jdk-version 11)
      (when-let [requested-src-version (System/getenv "JDK_SRC_VERSION")]
        (let [zip-path (format "base-src-%s.zip" requested-src-version)
              src-zip (clojure.java.io/file zip-path)]
          (if (.exists src-zip)
            (do (println "Found JDK sources:" src-zip)
                [src-zip])
            (do (println (format "%s not found. Run `make %s` to properly run all the tests."
                                 zip-path zip-path))
                nil)))))))

(def dev-test-common-profile
  {:dependencies '[[org.clojure/java.classpath "1.1.0"]
                   [nubank/matcher-combinators "3.9.1"
                    :exclusions [org.clojure/clojure]]]
   :source-paths (cond-> ["test-java" "java"]
                   ;; We only include sources with JDK21 because we only
                   ;; repackage sources for that JDK. Sources from one JDK are
                   ;; not compatible with other JDK for our test purposes.
                   (>= jdk-version 11) (into @jdk-sources-archive))
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

             ;; Development tools
             :dev ~(-> dev-test-common-profile
                       (update :source-paths conj "dev")
                       (update :dependencies conj '[org.clojure/tools.namespace "1.5.0"]))

             :cljfmt {:plugins [[lein-cljfmt "0.9.2"]]
                      :cljfmt {:indents {merge-meta [[:inner 0]]}}}

             :clj-kondo {:plugins [[com.github.clj-kondo/lein-clj-kondo "2023.07.13"]]}

             :eastwood  {:plugins  [[jonase/eastwood "1.4.0"]]
                         :eastwood {:ignored-faults {:unused-ret-vals-in-try {orchard.java {:line 84}
                                                                              orchard.java.parser-next-test true}}
                                    :exclude-namespaces ~(when jdk8?
                                                           '[orchard.java.modules
                                                             orchard.java.parser-utils
                                                             orchard.java.parser-next
                                                             orchard.java.parser-next-test])}}})
