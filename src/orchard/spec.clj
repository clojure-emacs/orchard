(ns orchard.spec
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str]
   [clojure.walk :as walk]))

(defn- spec [sym & args]
  (when-let [f (resolve sym)]
    (try (apply f args) (catch Exception _))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These are all wrappers for Clojure Spec functions.                                   ;;
;; - clojure.spec (released between Clojure 1.8 and 1.9, but never included in Clojure) ;;
;; - clojure.spec.alpha (renamed from clojure.spec and included in Clojure 1.9)         ;;
;; - clojure.alpha.spec (spec-2, the new experimental version)                          ;;
;; We can't simply require the ns because it's existence depends on the Clojure version ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-spec [v]
  (or (spec 'clojure.alpha.spec/get-spec v)
      (spec 'clojure.spec.alpha/get-spec v)
      (spec 'clojure.spec/get-spec v)))

(defn describe [s]
  (or (spec 'clojure.alpha.spec/describe s)
      (spec 'clojure.spec.alpha/describe s)
      (spec 'clojure.spec/describe s)))

(defn registry []
  (apply merge
         (spec 'clojure.spec/registry)
         (spec 'clojure.spec.alpha/registry)
         (spec 'clojure.alpha.spec/registry)))

(defn form [s]
  (or (spec 'clojure.alpha.spec/form s)
      (spec 'clojure.spec.alpha/form s)
      (spec 'clojure.spec/form s)))

(defn gen [s]
  (or (spec 'clojure.alpha.spec/gen s)
      (spec 'clojure.spec.alpha/gen s)
      (spec 'clojure.spec/gen s)))

(defn generate [s]
  (let [gen (gen s)]
    (or (spec 'clojure.alpha.spec.gen/generate gen)
        (spec 'clojure.spec.gen.alpha/generate gen)
        (spec 'clojure.spec.gen/generate gen))))

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
