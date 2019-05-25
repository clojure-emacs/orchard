(defproject cider/orchard "0.5.0-beta4"
  :description "A fertile ground for Clojure tooling"
  :url "https://github.com/clojure-emacs/orchard"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :scm {:name "git" :url "https://github.com/clojure-emacs/orchard"}
  :dependencies [[org.tcrawley/dynapath "1.0.0"]
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

  :profiles {
             ;; Clojure versions matrix
             :provided {:dependencies [[org.clojure/clojure "1.10.0"]
                                       [org.clojure/clojure "1.10.0" :classifier "sources"]]}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]
                                  [org.clojure/clojure "1.8.0" :classifier "sources"]]}
             :1.9 {:dependencies [[org.clojure/clojure "1.9.0"]
                                  [org.clojure/clojure "1.9.0" :classifier "sources"]]}
             :1.10 {:dependencies [[org.clojure/clojure "1.10.0"]
                                   [org.clojure/clojure "1.10.0" :classifier "sources"]]}
             :master {:repositories [["snapshots"
                                      "https://oss.sonatype.org/content/repositories/snapshots"]]
                      :dependencies [[org.clojure/clojure "1.11.0-master-SNAPSHOT"]
                                     [org.clojure/clojure "1.11.0-master-SNAPSHOT" :classifier "sources"]]}

             :sysutils {:plugins [[lein-sysutils "0.2.0"]]}

             :test {:resource-paths ["test-resources"]}

             ;; DEV tools
             :dev {:dependencies [[pjstadig/humane-test-output "0.9.0"]]
                   :resource-paths ["test-resources"]
                   :plugins [[com.jakemccrary/lein-test-refresh "0.23.0"]]
                   :injections [(require 'pjstadig.humane-test-output)
                                (pjstadig.humane-test-output/activate!)]
                   :test-refresh {:changes-only true}}

             ;; CI tools
             :cloverage {:plugins [[lein-cloverage "1.0.11-SNAPSHOT"]]}

             :cljfmt {:plugins [[lein-cljfmt "0.6.4"]]
                      :cljfmt {:indents {as-> [[:inner 0]]
                                         with-debug-bindings [[:inner 0]]
                                         merge-meta [[:inner 0]]
                                         letfn [[:block 1] [:inner 2]]}}}

             :eastwood {:plugins [[jonase/eastwood "0.3.4"]]
                        :eastwood {:config-files ["eastwood.clj"]}}})
