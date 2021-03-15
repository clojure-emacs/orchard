(ns orchard.xref
  "Utilities for finding function dependencies and
  references."
  {:added "0.5"}
  (:require
   [orchard.query :as q]))

(defn- as-val
  "Convert `thing` to a function value."
  [thing]
  (cond
    (var? thing) (var-get thing)
    (symbol? thing) (var-get (find-var thing))
    (fn? thing) thing))

(defn fn-deps
  "Returns a set with all the functions invoked by `val`.
  `val` can be a function value, a var or a symbol."
  {:added "0.5"}
  [val]
  (let [val (as-val val)]
    (set (some->> val class .getDeclaredFields
                  (keep (fn [^java.lang.reflect.Field f]
                          (or (and (identical? clojure.lang.Var (.getType f))
                                   (java.lang.reflect.Modifier/isPublic (.getModifiers f))
                                   (java.lang.reflect.Modifier/isStatic (.getModifiers f))
                                   (-> f .getName (.startsWith "const__"))
                                   (.get f val))
                              nil)))))))

(defn- fn->sym
  "Convert a function value `f` to symbol."
  [f]
  (symbol (Compiler/demunge (.getName ^Class (type f)))))

(defn- as-var
  "Convert `thing` to a var."
  [thing]
  (cond
    (var? thing) thing
    (symbol? thing) (find-var thing)
    (fn? thing) (find-var (fn->sym thing))))

(defn fn-refs
  "Find all functions that refer `var`.
  `var` can be a function value, a var or a symbol."
  {:added "0.5"}
  [var]
  (let [var (as-var var)
        all-vars (q/vars {:ns-query {:project? true} :private? true})
        all-vals (map var-get all-vars)
        deps-map (zipmap all-vars (map fn-deps all-vals))]
    (map first (filter (fn [[_k v]] (contains? v var)) deps-map))))
