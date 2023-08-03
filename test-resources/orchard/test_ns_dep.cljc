(ns ^{:doc "Dependency of test-ns namespace"} orchard.test-ns-dep
  (:require
   [clojure.string :as string]))

(defn foo-in-dep [foo] :bar)

(def x ::dep-namespaced-keyword)

(def referred string/trim)
