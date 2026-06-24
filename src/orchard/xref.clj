(ns orchard.xref
  "Utilities for finding function dependencies and
  references."
  {:added "0.5"}
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [orchard.query :as q])
  (:import (java.lang.ref Reference)
           (java.lang.reflect Modifier)
           (java.util.function BiConsumer)))

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

(defn fn-deps-class
  "Returns a set with all the functions invoked by `v`.
  `v` can be a function class or a symbol."
  {:added "0.9"}
  [v]
  (let [^Class v (cond (class? v) v
                       (symbol? v) (resolve v))]
    (when (class? v) ;; maybe a non-class was resolved
      (into #{} (keep (fn [^java.lang.reflect.Field f]
                        (when (and (identical? clojure.lang.Var (.getType f))
                                   (Modifier/isPublic (.getModifiers f))
                                   (Modifier/isStatic (.getModifiers f))
                                   (str/starts-with? (.getName f) "const__"))
                          (.get f nil))))
            (.getDeclaredFields v)))))

(def ^:private ^java.util.concurrent.ConcurrentHashMap class-cache
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
          ;; Uses the implementation detail that the compiler always prefixes
          ;; names of lambdas with the name of its surrounding function class.
          deps (volatile! #{})
          _ (.forEach
             class-cache
             (reify BiConsumer
               (accept [_ k v]
                 (when (str/starts-with? k f-class-name)
                   (vswap! deps into (fn-deps-class (.get ^Reference v)))))))
          ;; if there's no deps the class is most likely AoT compiled,
          ;; try to access it directly
          result (if (empty? @deps)
                   (fn-deps-class (.getClass v))
                   @deps)]
      ;; Re-resolve all vars to pick up the freshest. This is important because
      ;; there can be two seemingly equal #'foo.bar/baz var objects in the
      ;; result. That can happen as one re-evaluates code and the old var hasn't
      ;; been GC'd yet.
      (into #{} (map (comp resolve symbol)) result))))

(defn fn-transitive-deps
  "Returns a set with all the functions invoked inside `v` or inside those functions.
  `v` can be a function value, a var or a symbol."
  {:added "0.9"}
  [v]
  (let [deps (fn-deps v)]
    (loop [checked #{}
           to-check (into [] deps)
           deps deps]
      (if (empty? to-check)
        deps
        (let [[current & remaining] to-check
              new-deps (fn-deps current)]
          (recur (conj checked current)
                 (into (vec remaining) (remove checked new-deps))
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
  [var]
  (let [var (as-var var)
        all-vars (doall (q/vars {:ns-query {:project? true} :private? true}))]
    (filterv (fn [v2] (contains? (fn-deps v2) var)) all-vars)))

;;; Protocol and multimethod implementations

(defn- cached-classes
  "Return the classes currently held in Clojure's `DynamicClassLoader` cache."
  []
  (let [acc (volatile! (transient []))]
    (.forEach class-cache
              (reify BiConsumer
                (accept [_ _k v]
                  (when-let [c (.get ^Reference v)]
                    (when (class? c)
                      (vswap! acc conj! c))))))
    (persistent! @acc)))

(defn- class->location
  "Best-effort source location for the Clojure type named `class-name`.
  Records and types expose a `->Name` factory var that carries :file/:line; we
  go through it because the class object itself doesn't keep that metadata.
  Returns a map with :file/:line/:column, or nil for Java classes and the like."
  [^String class-name]
  (when-let [dot (str/last-index-of class-name ".")]
    (let [ns-part (clojure.lang.Compiler/demunge (subs class-name 0 dot))
          tname (subs class-name (inc dot))
          factory (try (resolve (symbol ns-part (str "->" tname)))
                       (catch Throwable _ nil))]
      (when factory
        (not-empty (select-keys (meta factory) [:file :line :column]))))))

(defn protocol-impls
  "Return the implementations of protocol `p` (a protocol var or its value).
  Covers both `extend`/`extend-type`/`extend-protocol` targets and inline
  `defrecord`/`deftype` implementers - the latter found by scanning loaded
  classes for the protocol's generated interface.  Each entry is a map with
  :name and, when resolvable, :file/:line/:column.  Only loaded code is visible."
  {:added "0.43"}
  [p]
  (let [proto (if (var? p) (deref p) p)
        ^Class iface (:on-interface proto)
        extend-names (map #(.getName ^Class %) (extenders proto))
        inline-names (keep (fn [^Class c]
                             (let [n (.getName c)]
                               (when (and (not (identical? c iface))
                                          (not (str/starts-with? n "compile__stub."))
                                          ;; skip reify/anonymous classes
                                          (not (str/includes? n "$"))
                                          (.isAssignableFrom iface c))
                                 n)))
                           (cached-classes))]
    (->> (distinct (concat extend-names inline-names))
         (map (fn [n] (merge {:name n} (class->location n))))
         (sort-by :name)
         vec)))

(defn multimethod-dispatch-values
  "Return the dispatch values of multimethod `m` (a var or a `MultiFn`).
  Each value is rendered with `pr-str`.  Per-`defmethod` source locations are
  not available at runtime, so none are included."
  {:added "0.43"}
  [m]
  (let [mf (if (var? m) (deref m) m)]
    (->> (methods mf) keys (map pr-str) sort vec)))
