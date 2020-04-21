(ns ^{:doc "A test namespace"} orchard.test-ns
  (:refer-clojure :exclude [unchecked-byte while replace])
  (:require [clojure.string :refer [replace]]
            [orchard.test-ns-dep :as test-dep :refer [foo-in-dep]]
            [orchard.test-no-defs :as no-defs])
  #?(:cljs (:require-macros [orchard.test-macros :as test-macros :refer [my-add]])
     :clj  (:require [orchard.test-macros :as test-macros :refer [my-add]]))
  #?(:cljs (:import [goog.ui IdGenerator])))

(defrecord TestRecord [a b c])
(deftype TestType [])

(def x ::some-namespaced-keyword)

(defn issue-28
  []
  (str "https://github.com/clojure-emacs/cljs-tooling/issues/28"))

(defn test-public-fn
  []
  42)

(defn- test-private-fn
  []
  (inc (test-public-fn)))
