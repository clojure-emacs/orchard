(ns user
  (:require
   [clojure.java.javadoc :refer [javadoc]]
   [clojure.pprint :refer [pprint]]
   [clojure.reflect :refer [reflect]]
   [clojure.repl :refer [apropos dir doc find-doc pst source]]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.test :as test]
   [clojure.tools.namespace.repl :refer [refresh refresh-all clear refresh-dirs set-refresh-dirs]]))

(def jdk8?
  (->> "java.version" System/getProperty (re-find #"^1.8.")))

(cond->> ["dev" "src" "test"]
  jdk8?       (into ["src-jdk8"])
  (not jdk8?) (into ["src-newer-jdks"
                     "test-newer-jdks"])
  true        (apply set-refresh-dirs))
