(ns orchard.namespace
  "Utilities for resolving and loading namespaces."
  {:author "Jeff Valk"
   :added "0.5.0"}
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [orchard.java.classpath :as cp]
   [orchard.misc :as misc])
  (:import
   (clojure.lang Namespace)
   (java.io File PushbackReader)))

;;; Namespace/source resolution

(defn read-namespace
  "Returns the namespace name from the first top-level `ns` form in the file."
  [url]
  (try
    (with-open [r (PushbackReader. (io/reader url))]
      (->> (repeatedly #(edn/read r))
           (filter #(and (list? %) (= (first %) 'ns))) ; ns form
           (map second)                                ; ns name
           (first)))
    (catch Exception _)))

(defn canonical-source
  "Returns the URL of the source file for the namespace object or symbol,
  according to the canonical naming convention, if present on the classpath."
  [ns]
  (let [path (-> (str ns)
                 (str/replace "-" "_")
                 (str/replace "." "/"))]
    (or (io/resource (str path ".clj"))
        (io/resource (str path ".cljc")))))

;;; Namespace Loading

(defn ensure-namespace
  "Require `ns` (no-op if already loaded). Return the symbol if successful,
  and `nil` if this fails."
  [ns]
  (try (doto (symbol ns) require)
       (catch Exception _)))

;;; Filters

(def project-root
  (io/as-url (io/file (System/getProperty "user.dir"))))

(defn in-project?
  "Whether the URL is in the current project's directory."
  [url]
  (let [path (if (misc/os-windows?) (comp str/lower-case str) str)]
    (.startsWith (path url) (path project-root))))

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

(defn classpath-namespaces
  "Returns all namespaces defined in sources on the classpath or the specified
  classpath URLs."
  ([classpath-urls]
   (->> (mapcat cp/classpath-seq classpath-urls)
        (filter misc/clj-file?)
        (map (comp read-namespace io/resource))
        (filter identity)
        (sort)))
  ([]
   (classpath-namespaces (cp/classpath))))

(defn project-namespaces
  "Returns all namespaces defined in sources within the current project."
  []
  (->> (cp/classpath)
       (filter (every-pred misc/directory? in-project?))
       (classpath-namespaces)))

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
       (map ensure-namespace)
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
