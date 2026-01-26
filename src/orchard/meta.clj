(ns orchard.meta
  "Utility functions for extracting and manipulating metadata."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [orchard.cljs.analysis :as cljs-ana]
   [orchard.clojuredocs :as cljdocs]
   [orchard.misc :as misc]
   [orchard.namespace :as ns]
   [orchard.pp :as pp]
   [orchard.spec :as spec])
  (:import
   (clojure.lang Compiler LineNumberingPushbackReader Namespace Var)))

;;; ## Extractors

(defn- format-spec-descripton
  "Format the spec description to display each predicate on a new line."
  [description]
  (if (seq? description)
    (str "(" (->> description
                  (map pp/pprint-str)
                  str/join
                  str/trim-newline)
         ")")
    (->> description
         pp/pprint-str
         str/trim-newline)))

(defn format-spec
  "Return sequence of [role spec-description] pairs."
  [fnspec]
  (for [role [:args :ret :fn]
        :let [spec (get fnspec role)]
        :when spec]
    [role (-> spec spec/describe format-spec-descripton)]))

(declare var-name)

(defn- maybe-add-spec
  "If the var `v` has a spec has associated with it, assoc that into meta-map.
  The spec is formatted to avoid processing in the client (e.g. CIDER)."
  [meta-map v]
  (if-let [spec (when v (spec/spec-form (var-name v)))]
    (merge meta-map {:spec spec})
    meta-map))

(defn- maybe-add-url
  "If `info` has a non blank :url or it's a :special-form build a :url
  entry pointing to https://clojure.org/..."
  [info]
  (if-let [url (cond
                 (not (str/blank? (:url info)))
                 (str "https://clojure.org/" (:url info))

                 (:special-form info)
                 (str "https://clojure.org/special_forms#" (:name info)))]
    (assoc info :url url)
    info))

(defn- maybe-add-file
  "If `meta-map` has no :file, assoc the canonical namespace source file."
  [{:keys [file ns] :as meta-map}]
  ;; If we don't know its file, use the ns file.
  (if (and ns (or (not file)
                  (re-find #"/form-init[^/]*$" file)))
    (-> (dissoc meta-map :line)
        (assoc :file (some-> (ns/canonical-source ns) .getPath)))
    meta-map))

(defn- maybe-add-see-also
  "If the var `v` has a see-also has associated with it, assoc that into meta-map."
  [meta-map v]
  (if-let [see-also (try
                      (cljdocs/see-also (var-name v))
                      ;; Skip merging see-also if exception is thrown.
                      (catch Exception _ nil))]
    (merge meta-map {:see-also see-also})
    meta-map))

(defn- maybe-protocol
  [info]
  (if-let [prot-meta (meta (:protocol info))]
    (merge info (select-keys prot-meta [:file :line]))
    info))

(defn resolve-var
  "Resolve `ns` and `sym` to a var.
  The function is a simple wrapper around `clojure.core/ns-resolve`."
  [ns sym]
  {:pre [(symbol? ns) (symbol? sym)]}
  (when-let [ns (find-ns ns)]
    (try (ns-resolve ns sym)
         ;; Impl might try to resolve it as a class, which may fail
         (catch ClassNotFoundException _
           nil)
         (catch java.lang.NoClassDefFoundError _
           ;; This can happen if there is a java class with a camel cased name
           ;; that otherwise matches the name of a clojure namespace, on a case
           ;; indifferent filesystem, eg. on macOS.
           nil)
         ;; TODO: Preserve and display the exception info
         (catch Exception _
           nil))))

(defn resolve-aliases
  "Retrieve the ns aliases for `ns`.
  The function is a simple wrapper around `clojure.core/ns-alias`."
  [ns]
  {:pre [(symbol? ns)]}
  (when-let [ns (find-ns ns)]
    (ns-aliases ns)))

(defn resolve-munged-printed-var
  "Given a printed munged representation of Clojure function, try to resolve it as
  a var. Supports the following representations:
  - clojure.core$str
  - clojure.core$str.invoke
  - clojure.main$repl$fn__9119.invoke (resolves to named var, not internal lambda)
  - some.ns$eval1234$closing_over_fn__12345.invoke"
  [sym]
  (let [demunged (-> (Compiler/demunge (str sym))
                     (str/replace #"--\d+" ""))
        [_ wo-method] (re-matches #"(.+?)(?:\.(?:invoke|invokeStatic|doInvoke))?"
                                  demunged)
        [ns-str name-str] (->> (str/split wo-method #"/")
                               (remove #(re-matches #"fn|eval\d+" %)))
        ns (some-> ns-str not-empty symbol find-ns)
        resolved (when (and ns name-str)
                   (some->> name-str not-empty symbol (ns-resolve ns)))]
    (when (var? resolved)
      resolved)))

;; Even if things like catch or finally aren't clojure special
;; symbols we want to be able to talk about them.
;; They just map to a special symbol.
(def special-sub-symbs '{& fn*, catch try, finally try})

(defn repl-special-meta*
  "Return the REPL specials info."
  [sym]
  (or (when-let [f (misc/require-and-resolve 'clojure.repl/special-doc)]
        (f sym))
      (when-let [f (misc/require-and-resolve 'cljs.repl/special-doc)]
        (f sym))))

(def repl-special-meta
  "Return the REPL specials info. Memoized."
  (memoize repl-special-meta*))

;; What I find very confusing in Clojure documentation is the use of "special form" which is not a concept,
;; just an annotation on vars (always macros) that are special forms in other lisps.
;;
;; In Clojure, real "special forms" are called special symbols:
;;
;; user> (keys (. clojure.lang.Compiler specials))
;; (& let* monitor-exit case* fn* try reify* ... )
;;
;; and even if the official documentation says let is a special form,
;; it isn't special at all, let* is, while let is just a macro.
;;
;; Looking at Clojure code I couldn't find any use of :special-form annotation
;; apart from printing a label at the repl doc.
;; Here we only take care of special symbols, look var-meta for stuff like let, fn, defn, etc.
(defn special-sym-meta
  "Return info for the symbol if it's a special-symbol?, or nil otherwise."
  [sym]
  (let [orig-sym sym
        sym (get special-sub-symbs sym sym)
        compiler-special? (special-symbol? orig-sym)]
    (when-let [m (and compiler-special? (repl-special-meta sym))]
      (-> m
          (assoc :name orig-sym)
          (assoc :see-also (try
                             (cljdocs/see-also (format "clojure.core/%s" sym))
                             ;; Skip attaching see-also if exception is thrown.
                             (catch Exception _ nil)))
          maybe-add-url))))

(declare var-code)

(defn- merge-meta-for-indirect-var*
  [target-map var-ref var-meta-fn cljs-env]
  (let [clj? (not cljs-env)
        original-meta (var-meta-fn var-ref)
        interesting-meta-keys #{:doc :arglists :style/indent :indent}
        original-has-interesting-meta? (some interesting-meta-keys (keys original-meta))
        source-var-ref-from-value (delay
                                    (when clj?
                                      (let [v @var-ref]
                                        (when (var? v)
                                          v))))
        var-ns (:ns original-meta)
        var-ns (if clj?
                 (if (instance? Namespace var-ns)
                   var-ns
                   (some-> var-ns :ns find-ns))
                 var-ns)
        source-var-ref-from-reader (delay
                                     (try
                                       (let [{:keys [form]} (var-code var-ref :var-meta-fn var-meta-fn)]
                                         (when (seq? form)
                                           (let [indirect-var-as-symbol (last form)]
                                             (when (symbol? indirect-var-as-symbol)
                                               (if clj?
                                                 (ns-resolve var-ns indirect-var-as-symbol)
                                                 (cljs-ana/ns-resolve cljs-env var-ns indirect-var-as-symbol))))))
                                       (catch Exception _
                                         ;; IO might have failed in the `var-code` call
                                         nil)))
        source-var (when-not original-has-interesting-meta?
                     (or @source-var-ref-from-value
                         @source-var-ref-from-reader))]
    (merge target-map
           (when source-var
             (select-keys (var-meta-fn source-var)
                          (remove #(contains? original-meta %)
                                  interesting-meta-keys))))))

(defn merge-meta-for-indirect-var-clj
  "If `var-ref` is a var that proxies another var (expressed as a var, or as a symbol denoting a var),
  and `var-ref` lacks metadata that is present in its proxy var,
  copies metadata from the proxied var to `var-ref`.

  This is useful for when Clojure users code e.g. `(def foo bar)`, where `bar` has a docstring,
  and for whatever reason, they do not wish to keep in sync the docstring from #'bar to #'foo manually.

  Only important, safe-to-copy metadata is possibly copied: `:doc`, `:arglists`, `:style/indent`..."
  [target-map ^Var var-ref]
  (merge-meta-for-indirect-var* target-map var-ref meta nil))

(defn merge-meta-for-indirect-var-cljs
  "If `var-map` describes a var that proxies another var (expressed as a symbol denoting a var),
  and `var-map` lacks metadata that is present in its indirect var,
  copies metadata from the proxied var to `var-map`.

  This is useful for when Clojure users code e.g. `(def foo bar)`, where `bar` has a docstring,
  and for whatever reason, they do not wish to keep in sync the docstring from #'bar to #'foo manually.

  Only important, safe-to-copy metadata is possibly copied: `:doc`, `:arglists`, `:style/indent`..."
  [cljs-env var-map]
  (merge-meta-for-indirect-var* var-map var-map identity cljs-env))

(def var-meta-allowlist
  [:ns :name :doc :file :arglists :forms :macro :special-form
   :protocol :line :column :static :added :deprecated :resource :style/indent :indent])

;; TODO: Split the responsibility of finding meta and normalizing the meta map.
(defn var-meta
  "Return a map of metadata for var v.
  If `allowlist` is missing, use `var-meta-allowlist`."
  ([v] (var-meta v var-meta-allowlist))
  ([v allowlist]
   (when (var? v)
     (-> v
         meta
         maybe-protocol
         (select-keys (or allowlist var-meta-allowlist))
         not-empty
         maybe-add-file
         maybe-add-url
         (update :ns ns-name)
         (maybe-add-spec v)
         (maybe-add-see-also v)
         (merge-meta-for-indirect-var-clj v)))))

(defn meta+
  "Return special form or var's meta."
  [v]
  (or (special-sym-meta v)
      (meta v)))

(defn var-name
  "Return special form or var's namespace-qualified name as string."
  [v]
  (let [mta (meta+ v)]
    (if-let [ns (:ns mta)]
      (str (ns-name ns) "/" (:name mta))
      (name (:name mta)))))

(defn var-doc
  "Return special form or var's docstring, optionally limiting the number of
  sentences returned to n."
  ([v]
   (or (:doc (meta+ v))
       "(not documented)"))
  ([n v]
   (->> (-> (var-doc v)
            (str/replace #"\s+" " ") ; normalize whitespace
            (str/split #"(?<=\.) ")) ; split sentences
        (take n)
        (str/join " "))))

(defn var-code
  "Find the source of the var `v`.
  Return a map of the var's metadata (:file, :line, :column, etc.) as well as:
    - :form : The form, as read by `clojure.core/read`, and
    - :code : The source code of the form
  Return nil if the source of the var cannot be found."
  [v & {:keys [var-meta-fn]
        :or {var-meta-fn var-meta}}]
  (when-let [{:keys [file line column] :as var-meta} (var-meta-fn v)]
    ;; file can be either absolute (eg: functions that have been eval-ed with
    ;; C-M-x), or relative to some path on the classpath.
    (when-let [res (or (io/resource file)
                       (let [f (io/file file)]
                         (when (.exists f)
                           f)))]
      (with-open [rdr (LineNumberingPushbackReader. (io/reader res))]
        ;; Skip to the right line
        (dotimes [_ (dec line)]
          (.readLine rdr))

        ;; Reader that collects the code as a string. Adapted from
        ;; clojure.repl/source.
        (let [text     (StringBuilder.)
              collect? (atom false)
              pbr      (proxy [LineNumberingPushbackReader] [rdr]
                         (read []
                           (let [^LineNumberingPushbackReader this this ; remove reflection warning on proxy-super call
                                 i (proxy-super read)]
                             (when @collect?
                               (.append text (char i)))
                             i))
                         (unread [c]
                           (when @collect?
                             (.deleteCharAt text (dec (.length text))))
                           (let [^LineNumberingPushbackReader this this] ; remove reflection warning on proxy-super call
                             (proxy-super unread ^int c))))
              ;; Fix bogus column number of 1, which really means 0
              column   (if (= 1 column) 0 column)]

          ;; Give it the right line and column number. We can just set the
          ;; line number directly, but there is no setColumnNumber method, so
          ;; we have to hack it a bit.
          (.setLineNumber pbr (.getLineNumber rdr))
          (dotimes [_ column]
            (.read pbr))

          ;; Now start collecting the code
          (reset! collect? true)

          (let [form (read {} pbr)
                code (str text)]
            (assoc var-meta
                   :form form
                   :code code)))))))

(defn ns-file
  "Finds the path to the file defining this `ns`."
  ^String [ns]
  (or (some-> (ns-publics ns)
              first
              val
              var-meta
              :file)
      (some-> (ns/canonical-source ns)
              .getPath)))

(defn ns-meta
  [ns]
  (when ns
    (merge
     (meta ns)
     {:ns (ns-name ns)
      :name (ns-name ns)
      :file (ns-file ns)
      :line 1})))

;;; ## Manipulation

(defn merge-meta
  "Non-throwing version of (vary-meta obj merge metamap-1 metamap-2 ...).
  Like `vary-meta`, this only applies to immutable objects. For
  instance, this function does nothing on atoms, because the metadata
  of an `atom` is part of the atom itself and can only be changed
  destructively."
  {:style/indent 1}
  [obj & metamaps]
  (try
    (apply vary-meta obj merge metamaps)
    (catch Exception _e obj)))

(defn strip-meta
  "Strip meta from form.
  If keys are provided, strip only those keys."
  ([form] (strip-meta form nil))
  ([form keys]
   (if (and (instance? clojure.lang.IObj form)
            (meta form))
     (with-meta form (when keys (apply dissoc (meta form) keys)))
     form)))

(defn macroexpand-all
  "Like `clojure.walk/macroexpand-all`, but preserves and macroexpands
  metadata. Also store the original form (unexpanded and stripped of
  metadata) in the metadata of the expanded form under original-key."
  [form & [original-key]]
  (let [md (meta form)
        expanded (walk/walk #(macroexpand-all % original-key)
                            identity
                            (if (seq? form)
                              ;; Without this, `macroexpand-all`
                              ;; throws if called on `defrecords`.
                              (try (macroexpand form)
                                   (catch ClassNotFoundException _e form))
                              form))]
    (if md
      ;; Macroexpand the metadata too, because sometimes metadata
      ;; contains, for example, functions. This is the case for
      ;; deftest forms.
      (merge-meta expanded
        (macroexpand-all md)
        (when original-key
          ;; We have to quote this, or it will get evaluated by
          ;; Clojure (even though it's inside meta).
          {original-key (list 'quote (strip-meta form))}))
      expanded)))
