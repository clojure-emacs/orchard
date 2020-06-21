(ns orchard.info
  "Retrieve the info map from var and symbols."
  (:refer-clojure :exclude [qualified-symbol?])
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [orchard.cljs.analysis :as cljs-ana]
   [orchard.cljs.meta :as cljs-meta]
   [orchard.java :as java]
   [orchard.java.classpath :as cp]
   [orchard.meta :as m]
   [orchard.misc :as misc]
   [orchard.java.resource :as resource]
   [clojure.tools.trace :as trace]))

(defn normalize-params
  "Normalize the info params.

  If :sym is qualified we compute :sym-ns and :unqualified-sym.

  We always assoc :unqualified-sym by calling name on :sym.
  We always assoc :dialect defaulting to :clj."
  {:added "0.5"}
  [params]
  (let [{:keys [sym ns context-ns]} params]
    (cond-> params
      ;; If :sym is qualified, we have to use (name), cause:
      ;;   (namespace 'mount.core) ;;=> nil
      ;;   (name 'mount.core) ;;=> "mount.core
      (misc/qualified-symbol? sym)
      (assoc :sym-ns (misc/namespace-sym sym)
             :qualified-symbol? true)

      true
      (update :dialect #(or % :clj))

      true
      (assoc :unqualified-sym (misc/name-sym sym)))))

;; (defn referred-meta
;;   [{:keys [ns sym-ns qualified-symbol? unqualified-sym]}]

;;   (let [refer-meta (some-> ns
;;                            (m/resolve-refer unqualified-sym)
;;                            (m/var-meta))]
;;     (println "----" sym-ns unqualified-sym qualified-symbol? (:ns refer-meta))
;;     (cond
;;       (and refer-meta qualified-symbol? (= sym-ns (:ns refer-meta)))
;;       refer-meta

;;       (and refer-meta (not qualified-symbol?))
;;       refer-meta

;;       :else nil)))

(defn clj-meta
  {:added "0.5"}
  [{:keys [dialect ns sym sym-ns qualified-symbol? unqualified-sym] :as params}]
  {:pre [(= dialect :clj)]}
  (let [ns (or ns sym-ns)]
    (println "####" ns sym sym-ns unqualified-sym)
    (or
     ;; it's a special (special-symbol?)
     (trace/trace "special" (m/special-sym-meta sym))

     ;; it's a referred symbol
     ;;   (!) refer should never resolve qualified symbols - we let m/resolve-var do that
     (trace/trace "refer" (some-> ns (m/resolve-refer sym) (m/var-meta)))

     ;; it's an alias for another ns
     (trace/trace "alias" (some-> ns (m/resolve-aliases) (get unqualified-sym) (m/ns-meta)))

     ;; it's a namespace symbol
     ;;   (!) We use :unqualified-sym *exclusively* here because because our :ns is
     ;;       too ambiguous.
     ;;
     ;;       Observe the following incorrect behavior (should return nil, there is a test):
     ;;       (info '{:ns clojure.core :sym non-existing}) ;;=> {:author "Rich Hickey" :ns clojure.core ...}
     (trace/trace "namespace" (some-> (find-ns unqualified-sym) (m/ns-meta)))

     ;; it's a var
     ;;   (!) has to come before Java resolution - see Integer/max test
     (trace/trace "var" (some-> ns (m/resolve-var sym) (m/var-meta)))

     ;; it's a Java class/member symbol
     (trace/trace "java" (some-> ns (java/resolve-symbol sym))))))

(defn cljs-meta
  {:added "0.5"}
  [{:keys [dialect ns sym env context-ns unqualified-sym]}]
  {:pre [(= dialect :cljs)]}
  (let [context-ns (or context-ns ns)]
    (or
     ;; a special symbol - always use :unqualified-sym
     (some-> (cljs-ana/special-meta env unqualified-sym)
             (cljs-meta/normalize-var-meta))
     ;; an NS
     (some->> (cljs-ana/find-ns env sym)
              (cljs-meta/normalize-ns-meta))
     ;; ns alias
     (some->> (cljs-ana/ns-alias env sym context-ns)
              (cljs-ana/find-ns env)
              (cljs-meta/normalize-ns-meta))
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

(defn info*
  "Provide the info map for the input ns and sym.

  The default dialect is :clj but it can be specified as part of params.

  Note that the :cljs dialect requires the compiler state to be passed
  in as :env key in params."
  [params]
  (let [params  (normalize-params params)
        meta    (condp = (:dialect params)
                  :clj  (clj-meta params)
                  :cljs (cljs-meta params))]

    ;; TODO: Split the responsibility of finding meta and normalizing the meta map.
    (some->
     meta

     (merge (when-let [file-path (:file meta)]
              {:file (cond-> file-path
                       (misc/boot-project?) cp/classpath-file-relative-path)})))))

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
