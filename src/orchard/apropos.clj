(ns orchard.apropos
  "Search symbols and docs matching a regular expression"
  {:author "Jeff Valk"}
  (:require [orchard.meta :refer [var-name var-doc] :as m]
            [orchard.query :as query])
  (:import [clojure.lang MultiFn]))

;;; ## Overview
;;
;; This namespace provides regular expression search across namespaces for
;; both symbols and documentation. Results ordered for the most common usages:
;; symbols from the current namespace are preferred, then `clojure.*` symbols,
;; and then other results.

;;; ## Symbol Search

(defn- apropos-sort
  "Return a list of vars, ordered with `ns` first,
  followed by `clojure.*` namespaces, and then all others sorted
  alphabetically."
  [ns vars]
  (let [clojure-ns? #(.startsWith (str (ns-name %)) "clojure.")]
    (sort-by
     (comp :ns meta)
     (fn [x y]
       (cond
         (nil? x) 1
         (nil? y) -1
         (= x ns) -1
         (= y ns)  1
         (and (clojure-ns? x) (not (clojure-ns? y))) -1
         (and (clojure-ns? y) (not (clojure-ns? x)))  1
         :else (compare (str x) (str y))))
     vars)))

(defn find-symbols
  "Takes a map and returns a list of maps containing name, doc and type.
  `:var-query` See `vars`.
  `:full-doc?` Causes the full doc to be returned instead of the abbreviated form.
  `:ns` If provided, the results will be sorted to show this namespace first."
  [{:keys [ns full-doc? var-query]}]
  (let [var-doc*    (if full-doc? var-doc (partial var-doc 1))]
    (->> (query/vars
          (assoc var-query
                 :manipulate-vars
                 (fn [nss vars]
                   (if (first (filter #(= (find-ns 'clojure.core) %) nss))
                     (concat m/special-forms vars)
                     vars))))
         (apropos-sort ns)
         (map (fn [v]
                {:name (var-name v)
                 :doc  (var-doc* v)
                 :type (cond (special-symbol? v) :special-form
                             (:macro (meta v)) :macro
                             (or (fn? @v) (instance? MultiFn @v)) :function
                             :else :variable)})))))
