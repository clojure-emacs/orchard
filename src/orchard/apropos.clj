(ns orchard.apropos
  "Search symbols and docs matching a regular expression"
  {:author "Jeff Valk"}
  (:require
   [orchard.meta :refer [var-name var-doc] :as m]
   [orchard.query :as query]
   [orchard.misc :as misc])
  (:import
   [clojure.lang MultiFn]))

;;; ## Overview
;;
;; This namespace provides regular expression search across namespaces for
;; both symbols and documentation. Results ordered for the most common usages:
;; symbols from the current namespace are preferred, then `clojure.*` symbols,
;; and then other results.

;;; ## Symbol Search

(defn- safe-comparator [x y]
  (compare (pr-str x) (pr-str y)))

(defn- default-comparator [ns clojure-ns?]
  (fn [x y]
    (cond
      (= x y)  0
      (nil? x) 1
      (nil? y) -1
      (= x ns) -1
      (= y ns)  1
      (and (clojure-ns? x) (not (clojure-ns? y))) -1
      (and (clojure-ns? y) (not (clojure-ns? x)))  1
      :else (safe-comparator x y))))

(defn apropos-sort
  "Return a list of vars, ordered with `ns` first,
  followed by `clojure.*` namespaces, and then all others sorted
  alphabetically."
  [ns vars]
  (assert (every? (some-fn class? var? symbol?) vars)
          (pr-str vars))
  (let [clojure-ns? #(.startsWith (str (ns-name %)) "clojure.")
        key-fn (comp :ns meta)]
    ;; https://clojure.org/guides/comparators
    (try
      (sort-by key-fn (default-comparator ns clojure-ns?) vars)
      ;; Handle https://github.com/clojure-emacs/orchard/issues/128
      (catch IllegalArgumentException e
        (when (System/getProperty "orchard.internal.test-suite-running")
          ;; Don't accept this exception in our CI - we should fix this if it's reproducible.
          (throw e))
        ;; Fallback to a simpler comparator:
        (sort-by key-fn safe-comparator vars)))))

(defn find-symbols
  "Takes a map and returns a list of maps containing name, doc and type.
  `:var-query` See `#'query/vars`.
  `:full-doc?` Causes the full doc to be returned instead of the abbreviated form.
  `:ns` If provided, the results will be sorted to show this namespace first."
  [{:keys [ns full-doc? var-query]}]
  (let [var-doc*    (if full-doc? var-doc (partial var-doc 1))]
    (->> (query/vars
          (assoc var-query
                 :manipulate-vars
                 (fn [nss vars]
                   (if (first (filter #(= (find-ns 'clojure.core) %) nss))
                     (concat (keys (or (misc/require-and-resolve 'clojure.repl/special-doc-map)
                                       (misc/require-and-resolve 'cljs.repl/special-doc-map)))
                             '[& catch finally]
                             vars)
                     vars))))
         (apropos-sort ns)
         (map (fn [v]
                {:name (var-name v)
                 :doc  (var-doc* v)
                 :type (cond (special-symbol? v) :special-form
                             (:macro (meta v)) :macro
                             (or (fn? @v) (instance? MultiFn @v)) :function
                             :else :variable)})))))
