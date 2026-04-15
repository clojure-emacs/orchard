(ns build
  (:require [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]))

(def lib 'cider/orchard)
(def version (or (not-empty (System/getenv "PROJECT_VERSION")) "0.0.0"))
(def class-dir "target/classes")
(def test-class-dir "target/test-classes")
(def jar-file (format "target/%s-%s.jar" (name lib) version))
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn clean [_]
  (b/delete {:path "target"}))

(defn compile-java [_]
  (b/javac {:src-dirs ["java"]
            :class-dir class-dir
            :basis @basis
            :javac-opts ["-Xlint:unchecked"]}))

(defn compile-test-java [_]
  (b/javac {:src-dirs ["test-resources/java"]
            :class-dir test-class-dir
            :basis (b/create-basis {:project "deps.edn" :aliases [:test]})
            :javac-opts ["-Xlint:unchecked"]}))

(defn jar [_]
  (compile-java nil)
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis @basis
                :src-dirs ["src"]
                :scm {:url "https://github.com/clojure-emacs/orchard"
                      :connection "scm:git:git://github.com/clojure-emacs/orchard.git"
                      :developerConnection "scm:git:ssh://git@github.com/clojure-emacs/orchard.git"
                      :tag (str "v" version)}
                :pom-data [[:description "A fertile ground for Clojure tooling"]
                           [:url "https://github.com/clojure-emacs/orchard"]
                           [:licenses
                            [:license
                             [:name "Eclipse Public License"]
                             [:url "http://www.eclipse.org/legal/epl-v10.html"]]]]})
  (b/copy-dir {:src-dirs ["src" "java" "resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))

(defn install [_]
  (jar nil)
  (b/install {:basis @basis
              :lib lib
              :version version
              :jar-file jar-file
              :class-dir class-dir}))

(defn deploy [_]
  (jar nil)
  (dd/deploy {:installer :remote
              :artifact (b/resolve-path jar-file)
              :pom-file (b/pom-path {:lib lib :class-dir class-dir})}))
