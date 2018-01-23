(defproject cider/orchard "0.1.0-SNAPSHOT"
  :description "A fertile ground for Clojure tooling"
  :url "https://github.com/clojure-emacs/orchard"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.tcrawley/dynapath "1.0.0"]
                 [org.clojure/java.classpath "0.2.3"]
                 [org.clojure/tools.namespace "0.3.0-alpha4"]]
  :exclusions [org.clojure/clojure] ; see versions matrix below

  :test-selectors {:java9.0 (complement :java9-excluded)}

  :profiles {
             ;; Clojure versions matrix
             :1.7 {:dependencies [[org.clojure/clojure "1.7.0"]]}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :1.9 {:dependencies [[org.clojure/clojure "1.9.0"]]}
             :master {:repositories [["snapshots"
                                      "https://oss.sonatype.org/content/repositories/snapshots"]]
                      :dependencies [[org.clojure/clojure "1.10.0-master-SNAPSHOT"]]}

             ;; CI tools
             :cloverage {:plugins [[lein-cloverage "1.0.11-SNAPSHOT"]]}

             :cljfmt {:plugins [[lein-cljfmt "0.5.7"]]
                      :cljfmt {:indents {as-> [[:inner 0]]
                                         with-debug-bindings [[:inner 0]]
                                         merge-meta [[:inner 0]]}}}

             :eastwood {:plugins [[jonase/eastwood "0.2.5"]]
                        :eastwood {:config-files ["eastwood.clj"]}}})
