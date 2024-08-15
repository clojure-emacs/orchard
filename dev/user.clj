(ns user
  {:clj-kondo/config '{:linters {:unused-namespace {:level :off}
                                 :unused-referred-var {:level :off}}}}
  (:require
   [clojure.java.javadoc :refer [javadoc]]
   [clojure.pprint :refer [pprint]]
   [clojure.reflect :refer [reflect]]
   [clojure.repl :refer [apropos dir doc find-doc pst source]]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.test :as test]
   [clojure.tools.namespace.repl :refer [clear refresh refresh-all refresh-dirs set-refresh-dirs]]))

(def jdk8?
  (->> "java.version" System/getProperty (re-find #"^1.8.")))

(cond->> ["dev" "src" "test"]
  (not jdk8?) (into ["test-newer-jdks"])
  true        (apply set-refresh-dirs))
