(ns ^{:doc "ClojureScript analysis functions."
      :author "Gary Trakhman"
      :added "0.6.0"}
 orchard.cljs.analysis
  (:require [orchard.misc :as u])
  (:refer-clojure :exclude [find-ns find-var all-ns ns-aliases]))

(defn all-ns
  [env]
  (->> (:cljs.analyzer/namespaces env)
       ;; recent CLJS versions include data about macro namespaces in the
       ;; compiler env, but we should not include them in completions or pass
       ;; them to format-ns unless they're actually required (which is handled
       ;; by macro-ns-candidates below)
       (into {} (filter (fn [[_ ns]]
                          (not (and (contains? ns :macros)
                                    (= 1 (count ns)))))))))

(defn find-ns
  [env ns]
  (get (all-ns env) ns))

;; Code adapted from clojure-complete (http://github.com/ninjudd/clojure-complete)

(defn imports
  "Returns a map of [import-name] to [ns-qualified-import-name] for all imports
  in the given namespace."
  [env ns]
  (:imports (find-ns env ns)))

(defn ns-aliases
  "Returns a map of [ns-name-or-alias] to [ns-name] for the given namespace."
  [env ns]
  (let [imports (imports env ns)]
    (->> (find-ns env ns)
         :requires
         (filter #(not (contains? imports (key %))))
         (into {}))))

(defn macro-ns-aliases
  "Returns a map of [macro-ns-name-or-alias] to [macro-ns-name] for the given namespace."
  [env ns]
  (:require-macros (find-ns env ns)))

(defn- expand-refer-map
  [m]
  (into {} (for [[k v] m] [k (symbol (str v "/" k))])))

(defn referred-vars
  "Returns a map of [var-name] to [ns-qualified-var-name] for all referred vars
  in the given namespace."
  [env ns]
  (->> (find-ns env ns)
       :uses
       expand-refer-map))

(defn referred-macros
  "Returns a map of [macro-name] to [ns-qualified-macro-name] for all referred
  macros in the given namespace."
  [env ns]
  (->> (find-ns env ns)
       :use-macros
       expand-refer-map))

(defn ns-alias
  "If sym is an alias to, or the name of, a namespace referred to in ns, returns
  the name of the namespace; else returns nil."
  [env sym ns]
  (get (ns-aliases env ns) sym))

(defn macro-ns-alias
  "If sym is an alias to, or the name of, a macro namespace referred to in ns,
  returns the name of the macro namespace; else returns nil."
  [env sym ns]
  (get (macro-ns-aliases env ns) sym))

(defn- public?
  [[_ var]]
  (not (:private var)))

(defn- named?
  [[_ var]]
  (not (:anonymous var)))

(defn- foreign-protocol?
  [[_ var]]
  (and (:impls var)
       (not (:protocol-symbol var))))

(defn- macro?
  [[_ var]]
  (:macro (meta var)))

(defn ns-vars
  "Returns a list of the vars declared in the ns."
  [env ns]
  (->> (find-ns env ns)
       :defs
       (filter (every-pred named? (complement foreign-protocol?)))
       (into {})))

(defn public-vars
  "Returns a list of the public vars declared in the ns."
  [env ns]
  (->> (find-ns env ns)
       :defs
       (filter (every-pred named? public? (complement foreign-protocol?)))
       (into {})))

(defn public-macros
  "Given a namespace return all the public var analysis maps. Analagous to
  clojure.core/ns-publics but returns var analysis maps not vars.

  Inspired by the ns-publics in cljs.analyzer.api."
  [env ns]
  {:pre [(symbol? ns)]}
  #?(:clj (when (and ns (clojure.core/find-ns ns))
            (->> (ns-publics ns)
                 (filter macro?)
                 (into {})))
     :cljs (->> (merge
                 (get-in env [:cljs.analyzer/namespaces ns :macros])
                 (get-in env [:cljs.analyzer/namespaces ns :defs]))
                (remove (fn [[k v]] (:private v)))
                (into {}))))

(defn core-vars
  "Returns a list of cljs.core vars visible to the ns."
  [env ns]
  (let [vars (public-vars env 'cljs.core)
        excludes (:excludes (find-ns env ns))]
    (apply dissoc vars excludes)))

(defn core-macros
  "Returns a list of cljs.core macros visible to the ns."
  [env ns]
  (let [macros (public-macros env #?(:clj 'cljs.core :cljs 'cljs.core$macros))
        excludes (:excludes (find-ns env ns))]
    (apply dissoc macros excludes)))

(defn ns-interns-from-env
  "Given a namespace return all the var analysis maps. Analagous to
  clojure.core/ns-interns but returns var analysis maps not vars.

  Directly from cljs.analyzer.api."
  [env ns]
  {:pre [(symbol? ns)]}
  (merge
   (get-in env [:cljs.analyzer/namespaces ns :macros])
   (get-in env [:cljs.analyzer/namespaces ns :defs])))

(defn sanitize-ns
  "Add :ns from :name if missing."
  [m]
  (-> m
      (assoc :ns (or (:ns m) (:name m)))
      (update :ns u/namespace-sym)
      (update :name u/name-sym)))

(defn var-meta
  "Return meta for the var, we wrap it in order to support both JVM and
  self-host."
  [var]
  (cond-> {}
    (map? var) (merge var)
    (var? var) (-> (merge (meta var))
                   (update :ns #(cond-> % (u/ns-obj? %) ns-name)))
    true sanitize-ns
    #?@(:cljs [true (-> (update :ns u/remove-macros)
                        (update :name u/remove-macros))])))

(defn ns-meta
  "Return meta for the var, we wrap it in order to support both JVM and
  self-host."
  [var]
  (cond-> {}
    (map? var) (merge var)
    (u/ns-obj? var) (merge {:ns (ns-name var)
                            :name (ns-name var)})
    true sanitize-ns
    #?@(:cljs [true (-> (update :ns u/remove-macros)
                        (update :name u/remove-macros))])))

(defn find-symbol-meta
  "Given a namespace-qualified var name, gets the analyzer metadata for that
  var."
  [env sym]
  (let [ns (find-ns env (u/namespace-sym sym))]
    (some-> (:defs ns)
            (get (u/name-sym sym))
            var-meta)))

(defn special-meta
  "Given a special symbol, gets the analyzer metadata."
  [_ sym]
  (when-let [meta #?(:clj (or (get (u/require-and-resolve 'cljs.repl/special-doc-map) sym)
                              (get (u/require-and-resolve 'cljs.repl/repl-special-doc-map) sym))
                     :cljs (or (get special/special-doc-map sym)
                               (get special/repl-special-doc-map sym)))]
    (merge {:name sym
            :ns 'cljs.core
            :special-form true}
           meta)))
