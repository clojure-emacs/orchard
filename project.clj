(def jdk-version
  (let [v (System/getProperty "java.specification.version")]
    (if (.contains v ".") 8 (Integer/parseInt v))))
(def jdk8? (= jdk-version 8))

(def dev-test-common-profile
  {:dependencies '[[nubank/matcher-combinators "3.9.1"
                    :exclusions [org.clojure/clojure]]
                   [org.clojure/test.check "1.1.1"]]
   :source-paths ["test-resources/java"]
   :resource-paths ["test-resources"]
   :test-paths ["test"]
   :java-source-paths ["test-resources/java"]})

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

  :source-paths ["src" "java"]
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

             :test ~(merge
                     dev-test-common-profile
                     {:resource-paths ["test-resources"
                                       "test-resources/not-a.jar"
                                       "test-resources/java-invalid"
                                       "does-not-exist.jar"]})

             ;; Development tools
             :dev ~dev-test-common-profile

             :cljfmt {:plugins [[lein-cljfmt "0.9.2"]]
                      :cljfmt {:indents {merge-meta [[:inner 0]]}}}

             :clj-kondo {:plugins [[com.github.clj-kondo/lein-clj-kondo "2024.11.14"]]}

             :eastwood  {:plugins  [[jonase/eastwood "1.4.3"]]
                         :eastwood {:ignored-faults {:unused-ret-vals-in-try {orchard.java {:line 84}
                                                                              orchard.java.parser-next-test true}}
                                    :exclude-namespaces ~(when jdk8?
                                                           '[orchard.java.modules
                                                             orchard.java.parser-next
                                                             orchard.java.parser-next-test])}}})
