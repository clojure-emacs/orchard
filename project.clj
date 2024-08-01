(def jdk8? (->> "java.version" System/getProperty (re-find #"^1.8.")))

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

  :source-paths ["src" "src-jdk8" "src-newer-jdks"]
  :test-paths ~(cond-> ["test"]
                 (not jdk8?)
                 (conj "test-newer-jdks"))
  :java-source-paths ["java"]

  :javac-options ["-Xlint:unchecked"]

  :profiles {:provided {:dependencies [[org.clojure/clojure "1.11.3"]
                                       [org.clojure/clojure "1.11.3" :classifier "sources"]]}

             :1.10 {:dependencies [[org.clojure/clojure "1.10.3"]
                                   [org.clojure/clojure "1.10.3" :classifier "sources"]]}
             :1.11 {:dependencies [[org.clojure/clojure "1.11.3"]
                                   [org.clojure/clojure "1.11.3" :classifier "sources"]]}
             :1.12 {:dependencies [[org.clojure/clojure "LATEST"]
                                   [org.clojure/clojure "LATEST" :classifier "sources"]]}

             ;; Needed to test how Orchard behaves with Clojurescript on classpath.
             :cljs {:dependencies [[org.clojure/clojurescript "1.11.132"]]
                    :test-paths ["test-cljs"]}

             :test {:dependencies [[org.clojure/java.classpath "1.1.0"]
                                   [nubank/matcher-combinators "3.9.1"
                                    :exclusions [org.clojure/clojure]]]
                    :resource-paths ["test-resources"
                                     "not-a.jar"
                                     "test-java-invalid"
                                     "does-not-exist.jar"]
                    :java-source-paths ["test-java"]
                    ;; Initialize the cache verbosely, as usual, so that possible issues can be more easily diagnosed:
                    :jvm-opts
                    ["-Dorchard.initialize-cache.silent=false"
                     "-Dorchard.internal.test-suite-running=true"]
                    :test-paths ["test"]}

             ;; Running the tests with enrich-classpath doing its thing isn't compatible with `lein test`,
             ;; So we use cognitect.test-runner instead.
             :cognitest {:dependencies [[org.clojure/tools.namespace "1.5.0"
                                         :exclusions [org.clojure/clojure]]
                                        [org.clojure/tools.cli "1.1.230"]]
                         :source-paths ["submodules/test-runner/src"]
                         ;; This piece of middleware dynamically adds the test paths to a cognitect.test-runner main invocation.
                         :middleware [~(do
                                         (defn add-cognitest [{:keys [test-paths] :as project}]
                                           (assert (seq test-paths))
                                           (let [cmd (reduce into [["cognitect.test-runner"]
                                                                   (vec
                                                                    (interleave (take (count test-paths)
                                                                                      (repeat "--dir"))
                                                                                test-paths))
                                                                   ["--namespace-regex" (pr-str ".*")]])]
                                             (assoc-in project [:enrich-classpath :main] (clojure.string/join " " cmd))))
                                         `add-cognitest)]}

             ;; Development tools
             :dev {:dependencies [[org.clojure/tools.namespace "1.5.0"]]
                   :source-paths ["dev" "submodules/spec-alpha2/src/main/clojure"]
                   :resource-paths ["test-resources"]}

             :cljfmt {:plugins [[lein-cljfmt "0.9.2"]]
                      :cljfmt {:indents {merge-meta [[:inner 0]]}}}

             :clj-kondo {:plugins [[com.github.clj-kondo/lein-clj-kondo "2023.07.13"]]}

             :eastwood  {:plugins  [[jonase/eastwood "1.4.0"]]
                         :eastwood {:ignored-faults {:unused-ret-vals-in-try {orchard.java {:line 84}
                                                                              orchard.java.parser-next-test true}}
                                    :exclude-namespaces ~(cond-> []
                                                           jdk8?
                                                           (conj 'orchard.java.parser
                                                                 'orchard.java.parser-test
                                                                 'orchard.java.parser-utils
                                                                 'orchard.java.parser-next
                                                                 'orchard.java.parser-next-test)

                                                           (or (not jdk8?)
                                                               (not= "true"
                                                                     (System/getenv "ENRICH_CLASSPATH")))
                                                           (conj 'orchard.java.legacy-parser))}}

             :deploy {:source-paths [".circleci/deploy"]}})
