(ns orchard.info
  "Retrieve the info map from var and symbols."
  (:refer-clojure :exclude [qualified-symbol?])
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [orchard.classpath :as cp]
   [orchard.cljs.analysis :as cljs-ana]
   [orchard.cljs.meta :as cljs-meta]
   [orchard.java :as java]
   [orchard.meta :as m]
   [orchard.misc :as u]
   [orchard.resource :as resource]))

(defn normalize-ns-meta
  "Normalize cljs namespace metadata to look like a clj."
  [meta]
  (merge (select-keys meta [:doc :author])
         {:file (-> meta :defs first second :file)
          :line 1
          :name (:name meta)
          :ns (:name meta)}))

(defn qualify-sym
  "Qualify a symbol, if any in :sym, with :ns.

  Return nil if :sym is nil, attempting to generate a valid symbol even
  in case some :ns is missing."
  {:added "0.6.0"}
  [ns sym]
  (when sym (symbol (some-> ns str) (str sym))))

(defn qualified-symbol?
  "Return true if x is a symbol with a namespace

  This is only available from Clojure 1.9 so we backport it until we
  drop support for Clojure 1.8."
  {:added "0.6.0"}
  [x]
  (boolean (and (symbol? x) (namespace x) true)))

(defn normalize-params
  "Normalize the info params.

  If :sym is unqualified we assoc a :qualified-sym key with it. The
  namespace used is :ns first and then :context-ns.

  If :sym is already qualified with assoc a :computed-ns key
  and :unqualified-sym key.

  If :dialect is nil, we assoc :clj, our default."
  {:added "0.6.0"}
  [params]
  (let [{:keys [sym ns context-ns]} params]
    (cond-> (update params :dialect #(or % :clj))
      ;; If :sym is qualified, we have to use (name), cause:
      ;;   (namespace 'mount.core) ;;=> nil
      ;;   (name 'mount.core) ;;=> "mount.core
      (qualified-symbol? sym)
      (assoc :qualified-sym sym
             :unqualified-sym (u/name-sym sym)
             :computed-ns (u/namespace-sym sym))

      (and sym (not (qualified-symbol? sym)))
      (assoc :unqualified-sym (-> sym name symbol))

      ;; if :sym is missing we still assoc :unqualified-sym from :ns
      (and (not sym) ns)
      (assoc :unqualified-sym ns)

      (and sym (not (qualified-symbol? sym)) (or ns context-ns))
      (assoc :qualified-sym (qualify-sym (or ns context-ns) sym)))))

(defn clj-meta
  {:added "0.6.0"}
  [{:keys [dialect ns sym computed-ns unqualified-sym]}]
  {:pre [(= dialect :clj)]}
  (let [ns (or ns computed-ns)]
    (or
     ;; it's a special (special-symbol?)
     (m/special-sym-meta sym)
     ;; it's a var
     (some-> ns (m/resolve-var sym) (m/var-meta))
     ;; it's an unqualified sym for an aliased var
     (some-> ns (m/resolve-var unqualified-sym) (m/var-meta))
     ;; sym is an alias for another ns
     (some-> ns (m/resolve-aliases) (get sym) (m/ns-meta))
     ;; We use :unqualified-sym *exclusively* here because because our :ns is
     ;; too ambiguous.
     ;;
     ;; Observe the incorrect behavior (should return nil, there is a test):
     ;;
     ;;   (info '{:ns clojure.core :sym non-existing}) ;;=> {:author "Rich Hickey" :ns clojure.core ...}
     ;;
     (some-> (find-ns unqualified-sym) (m/ns-meta))
     ;; it's a Java class/member symbol...or nil
     (some-> ns (java/resolve-symbol sym)))))

(defn cljs-meta
  {:added "0.6.0"}
  [{:keys [dialect ns sym env context-ns unqualified-sym]}]
  {:pre [(= dialect :cljs)]}
  (let [context-ns (or context-ns ns)]
    (or
     ;; a special symbol - always use :unqualified-sym
     (some-> (cljs-ana/special-meta env unqualified-sym)
             (cljs-meta/normalize-var-meta))
     ;; an NS
     (some->> (cljs-ana/find-ns env sym)
              (normalize-ns-meta))
     ;; ns alias
     (some->> (cljs-ana/ns-alias env sym context-ns)
              (cljs-ana/find-ns env)
              (normalize-ns-meta))
     ;; macro ns
     (some->> (find-ns unqualified-sym)
              (cljs-meta/normalize-macro-ns env))

     ;; macro ns alias
     (some->> (cljs-meta/aliased-macro-var env sym context-ns)
              (cljs-meta/normalize-macro-ns env))
     ;; referred var
     (some->> (get (cljs-ana/referred-vars env context-ns) sym)
              (cljs-ana/find-symbol-meta env)
              (cljs-meta/normalize-var-meta))
     ;; referred macro
     (some->> (cljs-meta/referred-macro-meta env sym context-ns)
              (cljs-meta/normalize-macro-meta))
     ;; scoped var
     (some->> (cljs-meta/scoped-var-meta env sym context-ns)
              (cljs-meta/normalize-var-meta))
     ;; scoped macro
     (some->> (cljs-meta/scoped-macro-meta env sym context-ns)
              (cljs-meta/normalize-macro-meta))
     ;; var in cljs.core
     (some->> (get (cljs-ana/core-vars env context-ns) sym)
              (cljs-ana/var-meta)
              (cljs-meta/normalize-var-meta))
     ;; macro in cljs.core
     (some->> (cljs-meta/scoped-macro-meta env sym 'cljs.core)
              (cljs-meta/normalize-macro-meta)))))

(def see-also-data
  (edn/read-string (slurp (io/resource "see-also.edn"))))

(defn see-also*
  [{:keys [dialect unqualified-sym] :as m}]
  (when-not (= :cljs dialect)
    (some->> unqualified-sym
             (str)
             (symbol "clojure.core")
             (str)
             (get see-also-data)
             (mapv symbol)
             (filter resolve))))

(def ^{:doc "Augment the map with see-also information. We have to use the
resolved (real) namespace and name here"}
  see-also (memoize see-also*))

(defn info*
  "Provide the info map for the input ns and sym.

  The default dialect is :clj but it can be specified as part of params.

  Note that the :cljs dialect requires the compiler state to be passed
  in as :env key in params."
  [params]
  (let [params  (normalize-params params)
        dialect (:dialect params)]

    ;; TODO split up responsability of finding meta and normalizing the meta map
    (some->
     (cond
       (= dialect :clj)  (clj-meta params)
       (= dialect :cljs) (cljs-meta params))

     ;; do not merge see-also if the info was not found
     (merge (when-let [m (see-also params)]
              {:see-also m}))

     (update :file (fn [file-path]
                     (if (u/boot-project?)
                       (cp/classpath-file-relative-path file-path)
                       file-path))))))

(defn info
  "Provide the info map for the input ns and sym.

  The default dialect is :clj but it can be specified as part of params.

  Note that the :cljs dialect requires the compiler state to be passed
  in as :env key in params."
  ([ns sym]
   (info* {:ns ns :sym sym}))
  ([ns sym params]
   (info* (assoc params :ns ns :sym sym))))

(defn info-java
  [class member]
  (java/member-info class member))

(defn file-path
  "For a file path, return a URL to the file if it exists and does not
  represent a form evaluated at the REPL."
  [x]
  (when (seq x)
    (let [f (io/file x)]
      (when (and (.exists f)
                 (not (-> f .getName (.startsWith "form-init"))))
        (io/as-url f)))))

(defn file-info
  [path]
  (let [[resource-relative resource-full] (resource/resource-path-tuple path)]
    (merge {:file (or (file-path path) resource-full path)}
           ;; Classpath-relative path if possible
           (if resource-relative
             {:resource resource-relative}))))

(defn javadoc-info
  "Resolve a relative javadoc path to a URL and return as a map. Prefer javadoc
  resources on the classpath; then use online javadoc content for core API
  classes. If no source is available, return the relative path as is."
  [^String path]
  {:javadoc (java/resolve-javadoc-path path)})
