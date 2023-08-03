(ns ^{:doc "A test namespace"} orchard.test-ns
  (:refer-clojure :exclude [replace unchecked-byte while])
  (:require
   [clojure.string :as string :refer [replace]]
   [orchard.test-no-defs :as no-defs]
   [orchard.test-ns-dep :as test-dep :refer [foo-in-dep referred]])
  #?(:clj  (:require [orchard.test-macros :as test-macros :refer [my-add]])
     :cljs (:require-macros [orchard.test-macros :as test-macros :refer [my-add]]))
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

(defn source
  "Docstring"
  ([])
  ([a b c]))

(def indirect1
  source)

(def indirect2
  replace)

(def indirect3
  string/capitalize)

(def indirect4
  clojure.string/includes?)
