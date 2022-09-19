(ns orchard.spec
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [orchard.misc :as misc]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These are all wrappers for Clojure Spec functions.                                   ;;
;; - clojure.spec (released between Clojure 1.8 and 1.9, but never included in Clojure) ;;
;; - clojure.spec.alpha (renamed from clojure.spec and included in Clojure 1.9)         ;;
;; - clojure.alpha.spec (spec-2, the new experimental version)                          ;;
;; We can't simply require the ns because it's existence depends on the Clojure version ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; clojure.spec

(def ^:private clojure-spec-get-spec
  (misc/call-when-resolved 'clojure.spec/get-spec))

(def ^:private clojure-spec-describe
  (misc/call-when-resolved 'clojure.spec/describe))

(def ^:private clojure-spec-form
  (misc/call-when-resolved 'clojure.spec/form))

(def ^:private clojure-spec-gen
  (misc/call-when-resolved 'clojure.spec/gen))

(def ^:private clojure-spec-registry
  (misc/call-when-resolved 'clojure.spec/registry))

(def clojure-spec?
  "True if `clojure.spec` is supported, otherwise false."
  (some? (resolve (symbol "clojure.spec" "get-spec"))))

;; clojure.spec.alpha

(def ^:private clojure-spec-alpha-get-spec
  (misc/call-when-resolved 'clojure.spec.alpha/get-spec))

(def ^:private clojure-spec-alpha-describe
  (misc/call-when-resolved 'clojure.spec.alpha/describe))

(def ^:private clojure-spec-alpha-form
  (misc/call-when-resolved 'clojure.spec.alpha/form))

(def ^:private clojure-spec-alpha-gen
  (misc/call-when-resolved 'clojure.spec.alpha/gen))

(def ^:private clojure-spec-alpha-registry
  (misc/call-when-resolved 'clojure.spec.alpha/registry))

(def clojure-spec-alpha?
  "True if `clojure.spec.alpha` is supported, otherwise false."
  (some? (resolve (symbol "clojure.spec.alpha" "get-spec"))))

;; clojure.alpha.spec - spec-2

(def ^:private clojure-alpha-spec-get-spec
  (misc/call-when-resolved 'clojure.alpha.spec/get-spec))

(def ^:private clojure-alpha-spec-describe
  (misc/call-when-resolved 'clojure.alpha.spec/describe))

(def ^:private clojure-alpha-spec-form
  (misc/call-when-resolved 'clojure.alpha.spec/form))

(def ^:private clojure-alpha-spec-gen
  (misc/call-when-resolved 'clojure.alpha.spec/gen))

(def ^:private clojure-alpha-spec-registry
  (misc/call-when-resolved 'clojure.alpha.spec/registry))

(def clojure-alpha-spec?
  "True if `clojure.alpha.spec` is supported, otherwise false."
  (some? (resolve (symbol "clojure.alpha.spec" "get-spec"))))

(def spec?
  "True if `clojure.spec`, `clojure.spec.alpha` or`clojure.alpha.spec` is supported, otherwise false."
  (or clojure-spec? clojure-spec-alpha? clojure-alpha-spec?))

(defn- try-fn [f & args]
  (try (apply f args) (catch Exception _)))

(defn- ex-unable-to-resolve-spec [s]
  (ex-info (format "Unable to resolve spec: %s" s) {:s s}))

(defn get-spec [k]
  (or (clojure-alpha-spec-get-spec k)
      (clojure-spec-alpha-get-spec k)
      (clojure-spec-get-spec k)))

(defn describe [s]
  (or (try-fn clojure-alpha-spec-describe s)
      (try-fn clojure-spec-alpha-describe s)
      (try-fn clojure-spec-describe s)
      (throw (ex-unable-to-resolve-spec s))))

(defn registry []
  (apply merge
         (clojure-spec-registry)
         (clojure-spec-alpha-registry)
         (clojure-alpha-spec-registry)))

(defn form [s]
  (or (try-fn clojure-alpha-spec-form s)
      (try-fn clojure-spec-alpha-form s)
      (try-fn clojure-spec-form s)
      (throw (ex-unable-to-resolve-spec s))))

(defn gen [s]
  (or (try-fn clojure-alpha-spec-gen s)
      (try-fn clojure-spec-alpha-gen s)
      (try-fn clojure-spec-gen s)
      (throw (ex-unable-to-resolve-spec s))))

(def ^:private generate*
  "All Clojure Spec versions use test.check under the hood. So let's
  directly use its `generate` function instead of going through the
  various Spec versions again."
  (misc/call-when-resolved 'clojure.test.check.generators/generate))

(defn generate [s]
  (when-let [gen (gen s)]
    (generate* gen)))

;;; Utility functions

(defn str-non-colls
  "Given a form, convert all non collection childs to str."
  [form]
  (walk/postwalk #(if (coll? %)
                    %
                    (str %))
                 form))

(defn- ns-name->ns-alias
  "Return mapping from full namespace name to its alias in the given namespace."
  [^String ns]
  (if ns
    (reduce-kv (fn [m alias ns]
                 (assoc m (name (ns-name ns)) (name alias)))
               {}
               (ns-aliases (symbol ns)))
    {}))

(defn spec-list
  "Retrieves a list of all specs in the registry, sorted by ns/name.
  If filter-regex is not empty, keep only the specs with that prefix."
  ([filter-regex]
   (spec-list filter-regex nil))
  ([filter-regex ns]
   (let [ns-alias (ns-name->ns-alias ns)
         sorted-specs (->> (registry)
                           keys
                           (mapcat (fn [kw]
                                     ;; Return an aliased entry in the current ns (if any)
                                     ;; with the fully qualified keyword
                                     (let [keyword-ns (namespace kw)]
                                       (if (= ns keyword-ns)
                                         [(str kw) (str "::" (name kw))]
                                         (if-let [alias (ns-alias keyword-ns)]
                                           [(str kw) (str "::" alias "/" (name kw))]
                                           [(str kw)])))))
                           sort)]
     (if (not-empty filter-regex)
       (filter (fn [spec-symbol-str]
                 (let [checkable-part (if (.startsWith ^String spec-symbol-str ":")
                                        (subs spec-symbol-str 1)
                                        spec-symbol-str)]
                   (re-find (re-pattern filter-regex) checkable-part)))
               sorted-specs)
       sorted-specs))))

(defn get-multi-spec-sub-specs
  "Given a multi-spec form, call its multi method methods to retrieve
  its subspecs."
  [multi-spec-form]
  (let [[_ multi-method-symbol & _] multi-spec-form]
    (->> @(resolve multi-method-symbol)
         methods
         (map (fn [[spec-k method]]
                [spec-k (form (method nil))])))))

(defn add-multi-specs
  "Walk down a spec form and for every subform that is a multi-spec
  add its sub specs."
  [form]
  (walk/postwalk (fn [sub-form]
                   (if (and (coll? sub-form)
                            (symbol? (first sub-form))
                            (-> sub-form first name (= "multi-spec")))
                     (concat sub-form (get-multi-spec-sub-specs sub-form))
                     sub-form))
                 form))

(defn spec-from-string
  "Given a string like \"clojure.core/let\" or \":user/email\" returns
  the associated spec in the registry, if there is one."
  [s]
  (let [[spec-ns spec-kw] (str/split s #"/")]
    (if (.startsWith ^String spec-ns ":")
      (get-spec (keyword (subs spec-ns 1) spec-kw))
      (get-spec (symbol s)))))

(defn normalize-spec-fn-form
  "Given a form like (fn* [any-symbol] ... any-symbol...) replace fn* with fn
  and any occurrence of any-symbol with %."
  [[_ [sym] & r]]
  (concat '(clojure.core/fn [%])
          (walk/postwalk (fn [form]
                           (if (and (symbol? form) (= form sym))
                             '%
                             form))
                         r)))

(defn normalize-spec-form
  "Applys normalize-spec-fn-form to any fn* sub form."
  [sub-form]
  (walk/postwalk (fn [form]
                   (if (and (seq? form) (= 'fn* (first form)))
                     (normalize-spec-fn-form form)
                     form))
                 sub-form))

(defn- expand-ns-alias
  "Expand a possible ns aliased keyword into a fully qualified keyword."
  [^String ns ^String spec-name]
  (if (and ns (.startsWith spec-name "::"))
    (let [slash (.indexOf spec-name "/")]
      (if (= -1 slash)
        ;; This is a keyword in the current namespace
        (str ":" ns "/" (subs spec-name 2))

        ;; This is a keyword in an aliased namespace
        (let [[keyword-ns kw] (.split (subs spec-name 2) "/")
              aliases (ns-aliases (symbol ns))
              ns-name (some-> keyword-ns symbol aliases ns-name name)]
          (if ns-name
            (str ":" ns-name "/" kw)
            spec-name))))

    ;; Nothing to expand
    spec-name))

(defn spec-form
  "Given a spec symbol as a string, get the spec form and prepare it for
  a response."
  ([spec-name]
   (spec-form spec-name nil))
  ([spec-name ns]
   (when-let [spec (spec-from-string (expand-ns-alias ns spec-name))]
     (-> (form spec)
         add-multi-specs
         normalize-spec-form
         str-non-colls))))

(defn spec-example
  "Given a spec symbol as a string, returns a string with a pretty printed
  example generated by the spec."
  [spec-name]
  (with-out-str
    (-> (spec-from-string spec-name)
        generate
        pp/pprint)))
