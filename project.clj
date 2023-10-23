(def jdk8? (->> "java.version" System/getProperty (re-find #"^1.8.")))

(defproject cider/orchard (or (not-empty (System/getenv "PROJECT_VERSION"))
                              "0.0.0")
  :description "A fertile ground for Clojure tooling"
  :url "https://github.com/clojure-emacs/orchard"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :scm {:name "git" :url "https://github.com/clojure-emacs/orchard"}

  :exclusions [org.clojure/clojure ; see versions matrix below
               org.clojure/clojurescript]

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

  :profiles {:provided {:dependencies [[org.clojure/clojure "1.11.1"] ;; Clojure versions matrix
                                       [org.clojure/clojure "1.11.1" :classifier "sources"]
                                       [org.clojure/clojurescript "1.11.4"]]
                        :test-paths ["test-cljs"]}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]
                                  [org.clojure/clojure "1.8.0" :classifier "sources"]]}
             :1.9 {:dependencies [[org.clojure/clojure "1.9.0"]
                                  [org.clojure/clojure "1.9.0" :classifier "sources"]]}
             :1.10 {:dependencies [[org.clojure/clojure "1.10.3"]
                                   [org.clojure/clojure "1.10.3" :classifier "sources"]]}
             :1.11 {:dependencies [[org.clojure/clojure "1.11.1"]
                                   [org.clojure/clojure "1.11.1" :classifier "sources"]]}
             :master {:repositories [["snapshots"
                                      "https://oss.sonatype.org/content/repositories/snapshots"]]
                      :dependencies [[org.clojure/clojure "1.12.0-master-SNAPSHOT"]
                                     [org.clojure/clojure "1.12.0-master-SNAPSHOT" :classifier "sources"]]}


             :test {:dependencies [[org.clojure/java.classpath "1.0.0"]
                                   [nubank/matcher-combinators "3.8.8"]]
                    :resource-paths ["test-resources"
                                     "not-a.jar"
                                     "does-not-exist.jar"]
                    :java-source-paths ["test-java"]
                    ;; Initialize the cache verbosely, as usual, so that possible issues can be more easily diagnosed:
                    :jvm-opts
                    ["-Dorchard.initialize-cache.silent=false"
                     "-Dorchard.internal.test-suite-running=true"]
                    :test-paths ["test"]
                    :source-paths ["test-runner/src"]}

             ;; Running the tests with enrich-classpath doing its thing isn't compatible with `lein test`,
             ;; So we use cognitect.test-runner instead.
             :cognitest {:dependencies [[org.clojure/tools.namespace "1.4.4"]
                                        [org.clojure/tools.cli "1.0.206"]]
                         :source-paths ["test-runner/src"]
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
             :dev {:plugins [[cider/cider-nrepl "0.40.0"]
                             [refactor-nrepl "3.9.0"]]
                   :dependencies [[nrepl/nrepl "1.0.0"]
                                  [org.clojure/tools.namespace "1.4.4"]]
                   :source-paths ["dev" "src-spec-alpha-2/src/main/clojure"]
                   :resource-paths ["test-resources"]}

             :cljfmt {:plugins [[lein-cljfmt "0.9.2"]]
                      :cljfmt {:indents {as-> [[:inner 0]]
                                         with-debug-bindings [[:inner 0]]
                                         merge-meta [[:inner 0]]
                                         letfn [[:block 1] [:inner 2]]}}}

             :clj-kondo {:plugins [[com.github.clj-kondo/lein-clj-kondo "2023.07.13"]]}

             :eastwood  {:plugins  [[jonase/eastwood "1.4.0"]]
                         :eastwood {:ignored-faults {:unused-ret-vals-in-try {orchard.java {:line 84}
                                                                              orchard.java.parser-next-test true}}
                                    :exclude-namespaces ~(cond-> '[clojure.alpha.spec
                                                                   clojure.alpha.spec.gen
                                                                   clojure.alpha.spec.impl
                                                                   clojure.alpha.spec.test]
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
