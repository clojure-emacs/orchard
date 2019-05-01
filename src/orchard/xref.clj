(ns orchard.xref
  "Utilities for finding function dependencies and
  usages."
  {:added "0.5.0"}
  (:require
   [orchard.query :as q]))

(defn- as-val [thing]
  (cond
    (var? thing) (deref thing)
    (symbol? thing) (deref (find-var thing))
    (fn? thing) thing))

(defn fdeps
  "Returns a set with all the functions invoked by `val`.
  `val` must be a function value, a var or a symbol."
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

(defn xref
  "Find all functions that refer `var`.
  It can be either a var or a symbol that can resolved to a var."
  [var]
  (let [var (if (var? var) var (find-var var))
        all-vars (q/vars {:ns-query {:project? true} :private? true})
        all-vals (map deref all-vars)
        deps-map (zipmap all-vars (map fdeps all-vals))]
    (map first (filter (fn [[k v]] (contains? v var)) deps-map))))
