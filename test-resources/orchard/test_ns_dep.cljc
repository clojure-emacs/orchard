(ns ^{:doc "Dependency of test-ns namespace"} orchard.test-ns-dep
  (:require
   [clojure.string :as string]))

(defn foo-in-dep
  {:custom/meta 1}
  [foo]
  :bar)

(def x ::dep-namespaced-keyword)

(def referred string/trim)
