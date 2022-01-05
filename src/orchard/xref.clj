(ns orchard.xref
  "Utilities for finding function dependencies and
  references."
  {:added "0.5"}
  (:require
   [clojure.repl :as repl]
   [clojure.string :as str]
   [orchard.query :as q]))

(defn- as-val
  "Convert `thing` to a function value."
  [thing]
  (cond
    (var? thing) (var-get thing)
    (symbol? thing) (var-get (find-var thing))
    (fn? thing) thing))

(defn- fn-name [^java.lang.Class f]
  (-> f .getName repl/demunge symbol))

(defn- fn-deps-class
  [v]
  (let [^java.lang.Class v (if (class? v)
                             v
                             (eval v))]
    (set (some->> v .getDeclaredFields
                  (keep (fn [^java.lang.reflect.Field f]
                          (or (and (identical? clojure.lang.Var (.getType f))
                                   (java.lang.reflect.Modifier/isPublic (.getModifiers f))
                                   (java.lang.reflect.Modifier/isStatic (.getModifiers f))
                                   (-> f .getName (.startsWith "const__"))
                                   (.get f (fn-name v)))
                              nil)))))))

(defn fn-deps
  "Returns a set with all the functions invoked by `val`.
  `val` can be a function value, a var or a symbol."
  {:added "0.5"}
  [s]
  (when-let [^clojure.lang.AFn v (as-val s)]
    (let [f-class-name (-> v .getClass .getName)
          ;; breaks when called with
          ;; (.getDeclaredField clojure.lang.DynamicClassLoader "classCache")
          classCache (->> clojure.lang.DynamicClassLoader .getDeclaredFields second)
          classes (into {} (.get classCache clojure.lang.DynamicClassLoader))
          filtered-classes (->> classes
                                (filter (fn [[k _v]] (clojure.string/includes? k f-class-name)))
                                (map (fn [[_k v]] (.get ^java.lang.ref.Reference v))))
          deps (set (mapcat fn-deps-class filtered-classes))]
      deps)))

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
  [v]
  (let [var (as-var v)
        all-vars (q/vars {:ns-query {:project? true} :private? true})
        deps-map (zipmap all-vars (map fn-deps all-vars))]
    (map first (filter (fn [[_k v]] (contains? v var)) deps-map))))

(comment
  (nth (macroexpand (read-string "(def archive? (clojure.core/fn ([f] (file-ext? f .jar .zip))))")) 2)
  (fn-deps #'fn-refs)
  (fn-deps #'orchard.xref/fn->sym)
  (fn-refs #'orchard.xref/fn->sym)
  (into {} (.get (->> clojure.lang.DynamicClassLoader .getDeclaredFields second) clojure.lang.DynamicClassLoader))
  (def vars (q/vars {:ns-query {:project? true} :private? true}))
  (map fn-deps vars))