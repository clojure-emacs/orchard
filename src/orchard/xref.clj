(ns orchard.xref
  "Utilities for finding function dependencies and
  references."
  {:added "0.5"}
  (:require
   [clojure.repl :as repl]
   [clojure.set :as set]
   [clojure.string :as string]
   [orchard.query :as q]))

(defn- var->symbol
  ;; TODO: use `symbol` once we start targeting Clojure >= 1.10 after CIDER 1.8 is released.
  "Normally one could just use `(symbol var-ref)`,
  but that doesn't work in older Clojures."
  [var-ref]
  (let [{:keys [ns name]} (meta var-ref)]
    (symbol (str (ns-name ns))
            (str name))))

(defn- var->fn [var-ref]
  (let [{:keys [test]} (meta var-ref)]
    (if (fn? test)
      test ;; for deftests, :test metadata contains the actual test implementation, with all the interesting contents.
      (var-get var-ref))))

(defn- to-fn
  "Convert `thing` to a function value."
  [thing]
  (cond
    (var? thing) (var->fn thing)
    (symbol? thing) (var->fn (find-var thing))
    (fn? thing) thing))

(defn- fn-name [^java.lang.Class f]
  (-> f .getName repl/demunge symbol))

(def eval-lock
  "We don't want parallel evaluation - easily dangerous."
  (Object.))

(defn fn-deps-class
  "Returns a set with all the functions invoked by `v`.
  `v` can be a function class or a symbol."
  {:added "0.9"}
  [v]
  (let [^java.lang.Class v (if (class? v)
                             v
                             (locking eval-lock
                               (eval v)))]
    (into #{} (keep (fn [^java.lang.reflect.Field f]
                      (or (and (identical? clojure.lang.Var (.getType f))
                               (java.lang.reflect.Modifier/isPublic (.getModifiers f))
                               (java.lang.reflect.Modifier/isStatic (.getModifiers f))
                               (-> f .getName (.startsWith "const__"))
                               (.get f (fn-name v)))
                          nil))
                    (.getDeclaredFields v)))))

(def ^:private class-cache
  "Reference to Clojures class cache.
   This holds of classes compiled by the Clojure compiler,
   one class per function and one per repl eval.
   This field is package private, so it has to be set to
   accessible otherwise an IllegalAccess exception would
   be thrown."
  (let [classCache* (.getDeclaredField clojure.lang.DynamicClassLoader "classCache")]
    (.setAccessible classCache* true)
    (.get classCache* clojure.lang.DynamicClassLoader)))

(defn fn-deps
  "Returns a set with all the functions invoked inside `v` or any contained anonymous functions.
  `v` can be a function value, a var or a symbol.
   If a function was defined multiple times, old lambda deps will
   be returned.
   This does not return functions marked with meta :inline like `+`
   since they are already compiled away at this point."
  {:added "0.5"}
  [v]
  (when-let [^clojure.lang.AFn v (to-fn v)]
    (let [f-class-name (-> v .getClass .getName)
          ;; this uses the implementation detail that the clojure compiler always
          ;; prefixes names of lambdas with the name of its surrounding function class
          deps (into #{}
                     (comp (filter (fn [[k _v]]
                                     (clojure.string/includes? k f-class-name)))
                           (map (fn [[_k value]]
                                  (.get ^java.lang.ref.Reference value)))
                           (mapcat fn-deps-class))
                     class-cache)
          result
          ;; if there's no deps the class is most likely AoT compiled,
          ;; try to access it directly
          (if (empty? deps)
            (-> v .getClass fn-deps-class)
            deps)]
      (into #{}
            (map resolve) ;; choose the freshest one
            ;; group duplicates. This is important
            ;; because there can be two seemingly equal #'foo.bar/baz var objects in the result.
            ;; That can happen as one re-evaluates code and the old var hasn't been GC'd yet.
            (keys (group-by var->symbol result))))))

(defn fn-transitive-deps
  "Returns a set with all the functions invoked inside `v` or inside those functions.
  `v` can be a function value, a var or a symbol."
  {:added "0.9"}
  [v]
  (let [deps (fn-deps v)]
    (loop [checked #{}
           to-check (into [] deps)
           deps deps]
      (cond
        (empty? to-check) deps
        :else (let [[current & remaining] to-check
                    new-deps (fn-deps current)]
                (recur (conj checked current)
                       (concat remaining (filter #(contains? deps %) new-deps))
                       (set/union deps new-deps)))))))

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
        deps-map (zipmap all-vars (pmap fn-deps all-vars))]
    (into []
          (comp (filter (fn [[_k v]]
                          (contains? v var)))
                (map first))
          deps-map)))

(comment
  ;; this can be used to blow up memory, which will clear the class cache of old references
  (defn oom []
    (try (let [memKiller (java.util.ArrayList.)]
           (loop [free 10000000]
             (.add memKiller (object-array free))
             (.get memKiller 0)
             (recur 100000 #_(if (< (Math/abs (.. Runtime (getRuntime) (freeMemory))) Integer/MAX_VALUE)
                               (Math/abs (.. Runtime (getRuntime) (freeMemory)))
                               Integer/MAX_VALUE))))
         (catch OutOfMemoryError _
           (println "freed"))))

  (fn-deps #'fn-refs)
  (fn-deps #'orchard.xref/fn-deps)
  (fn-refs #'orchard.xref/fn->sym)

  ;; returns all classes in this ns, even repl eval'd
  (let [f-class-name "orchard.xref" #_(-> orchard.xref/fn-deps .getClass .getName)
        classes (into #{} (comp (filter (fn [[k _v]] (clojure.string/includes? k f-class-name)))
                                (map (fn [[_k v]] (.get ^java.lang.ref.Reference v))))
                      class-cache)]
    classes)

  (oom)
  (def vars (q/vars {:ns-query {:project? true} :private? true}))

  (map fn-deps vars))
