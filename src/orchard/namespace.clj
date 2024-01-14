(ns orchard.namespace
  "Utilities for resolving and loading namespaces.

  Operations are parallel wherever it makes sense and it's safe to do so;
  efficiency matters particularly for large projects/classpaths."
  {:author "Jeff Valk"
   :added "0.5"}
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [orchard.java.classpath :as cp]
   [orchard.misc :as misc])
  (:import
   (java.io File PushbackReader)
   (java.net URL)
   (java.nio.file FileVisitOption Files Path)
   (java.util.function BiPredicate)
   (java.util.stream Collectors)))

;;; Namespace/source resolution

(defn- ns-form->ns-name [x]
  (let [s (second x)]
    (when (symbol? s)
      s)))

(defn- ns-form? [x]
  (and (list? x)
       (-> x first #{`ns 'ns})))

(defn- read-ns [url pred extract]
  (with-open [r (PushbackReader. (io/reader url))]
    (loop []
      (let [found (try
                    (binding [*read-eval* false]
                      (read {:read-cond :allow
                             :eof ::eof}
                            r))
                    (catch Exception _
                      ::fail))]
        (cond
          (#{::eof ::fail} found)
          nil

          (pred found)
          (extract found)

          :else
          (recur))))))

(defn read-ns-name
  "Returns the namespace name from the first top-level `ns` form in the file."
  [url]
  (read-ns url
           (every-pred ns-form? ns-form->ns-name)
           ns-form->ns-name))

(defn read-namespace
  "Returns the namespace name from the first top-level `ns` form in the file."
  {:deprecated "0.19"}
  [url]
  (read-ns-name url))

(defn read-ns-form
  "Returns the first top-level `ns` form in the file."
  {:added "0.19"}
  [url]
  (read-ns url ns-form? identity))

(defn canonical-source
  "Returns the URL of the source file for the namespace object or symbol,
  according to the canonical naming convention, if present on the classpath."
  ^URL [ns]
  (let [path (-> (str ns)
                 (string/replace "-" "_")
                 (string/replace "." "/"))]
    (or (io/resource (str path ".clj"))
        (io/resource (str path ".cljc"))
        (io/resource (str path ".cljs")))))

;;; Namespace Loading

(defn ensure-namespace!
  "Require `ns` (no-op if already loaded). Return the symbol if successful,
  and `nil` if this fails."
  [ns]
  (try (doto (symbol ns) require)
       (catch Exception _)))

(defn ^:deprecated ensure-namespace
  "Renamed - please use `#'ensure-namespace!` instead."
  [ns]
  (ensure-namespace! ns))

;;; Filters

(def project-root
  (io/as-url (io/file (System/getProperty "user.dir"))))

(defn in-project?
  "Whether the URL is in the current project's directory."
  [url]
  (let [path (if (misc/os-windows?) (comp string/lower-case str) str)]
    (.startsWith ^String (path url) (path project-root))))

(defn inlined-dependency?
  "Returns true if the namespace matches one of our, or eastwood's,
   inlined dependencies."
  [namespace]
  (let [ns-name (str (ns-name namespace))]
    (or
     ;; rewritten by mranderson
     (.startsWith ns-name "deps.")
     (.startsWith ns-name "mranderson")
     (.startsWith ns-name "cider.nrepl.inlined-deps")
     ;; rewritten by dolly
     (.startsWith ns-name "eastwood.copieddeps"))))

(defn internal-namespace?
  "Returns true if the namespace matches the given prefixes."
  [namespace & [prefixes]]
  (let [ns-name (str (ns-name namespace))]
    (->> prefixes
         (map re-pattern)
         (map #(re-find % ns-name))
         (some (complement nil?)))))

(defn has-tests?
  "Returns a truthy value if the namespace has any vars with `:test` metadata."
  [ns]
  (seq (filter (comp :test meta val) (ns-interns ns))))

;;; Project Namespaces
;;
;; These methods search sources on the classpath. Non-classpath source
;; files, documentation code, etc within the project directory are ignored.

(defn jvm-clojure-resource-name->ns-form
  "Given a .clj or .cljc `resource-name`, returns its `ns` form."
  [resource-name]
  (when (misc/clj-file? resource-name)
    (some-> resource-name
            io/resource ;; can return nil for Emacs backup files, for example
            read-ns-form)))

(defn jvm-clojure-resource-name->ns-name
  "Given a .clj or .cljc `resource-name`, returns its namespace name."
  [resource-name]
  (when (misc/clj-file? resource-name)
    (some-> resource-name
            io/resource ;; can return nil for Emacs backup files, for example
            read-ns-name)))

(defn- find-clj+cljc-files-efficiently
  "Finds .clj/c files as efficiently as possible.

  In particular, avoids returning unnecessary files,
  and creating objects for those.

  This way, upstream consumers like `orchard.java.classpath/classpath-seq` will operate on fewer File objects,
  improving performance for large workloads (e.g. a directory with 1M files was placed as a resource)."
  [^File dir]
  (let [start-path (.toPath dir)
        matcher (reify BiPredicate
                  (test [_this path _attrs]
                    (boolean (and
                              (re-find #"\.cljc*$" (.toString ^Path path)) ;; operate directly on the Path (without File conversion) for efficiency
                              (-> ^Path path .toFile .isFile)))))]

    (->> (.collect (Files/find start-path 100 matcher (into-array FileVisitOption []))
                   (Collectors/toList))
         (mapv (fn [^Path path]
                 (.toFile path))))))

(defn classpath-namespaces
  "Returns all namespaces (by default: of .clj or .cljc extension)
  defined in sources on the classpath or the specified classpath URLs."
  ([]
   (classpath-namespaces (cp/classpath)))

  ([classpath-urls]
   (sort (classpath-namespaces classpath-urls jvm-clojure-resource-name->ns-name)))

  ([classpath-urls extract-fn]
   (->> classpath-urls
        (pmap (fn [classpath-url]
                (cp/classpath-seq classpath-url
                                  (when (= extract-fn jvm-clojure-resource-name->ns-name) ;; Prefer most efficient method when possible (#222)
                                    (fn [^File dir]
                                      (find-clj+cljc-files-efficiently dir))))))
        (apply concat)
        (pmap extract-fn)
        (filter identity))))

(defn project-namespaces
  "Returns all JVM Clojure namespaces defined in sources within the current project."
  []
  (->> (cp/classpath)
       (pmap (fn [x]
               (when ((every-pred misc/directory? in-project?) x)
                 x)))
       (filter identity)
       (classpath-namespaces)))

(defn project-ns-forms
  "Returns all JVM Clojure `ns` forms defined in sources within the current project,
  indexed by `ns-name`."
  {:added "0.19"}
  []
  (let [resources (->> (cp/classpath)
                       (pmap (fn [x]
                               (when ((every-pred misc/directory? in-project?) x)
                                 x)))
                       (filter identity))]
    (into {}
          (map (fn [ns-form]
                 [(ns-form->ns-name ns-form)
                  ns-form]))
          (classpath-namespaces resources jvm-clojure-resource-name->ns-form))))

(defn ns-form-imports
  "Given a `ns` form, returns its `:imports` as fully-qualified Java class names,
  expressed as symbols.

  Prefix notation is undone, so that all returned members are simple symbols
  that can be resolved to a class directly."
  {:added "0.19"}
  [ns-form]
  (->> ns-form
       (filter (every-pred list?
                           (comp #{:import} first)))
       (mapcat (fn [[_import-keyword & clauses]]
                 (->> clauses
                      (mapcat (fn [x]
                                (if (symbol? x)
                                  [x]
                                  (let [[prefix & classes] x]
                                    (map (fn [class-symbol]
                                           (symbol (str prefix
                                                        "."
                                                        class-symbol)))
                                         classes))))))))
       (distinct)
       (sort-by pr-str)
       (vec)))

(defn loaded-project-namespaces
  "Return all loaded namespaces defined in the current project."
  []
  (->> (project-namespaces)
       (filter (set (map ns-name (all-ns))))
       sort))

(defn load-project-namespaces
  "Require and return all namespaces validly defined in the current project."
  []
  (->> (project-namespaces)
       ;; don't pmap this - it performs a `require`:
       (map ensure-namespace!)
       (filter identity)
       sort))

(defn loaded-namespaces
  "Returns all loaded namespaces, except those coming from inlined dependencies.
  `filter-regexps` is used to filter out namespaces matching regexps."
  [& [filter-regexps]]
  (->> (all-ns)
       (remove inlined-dependency?)
       (remove #(internal-namespace? % filter-regexps))
       (map ns-name)
       (map name)
       (sort)))
