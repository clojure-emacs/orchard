(ns orchard.ns-form
  "Read the `ns` form of a source file and take it apart, without loading or
  evaluating anything."
  {:added "0.46"}
  (:require
   [clojure.java.io :as io])
  (:import
   (clojure.lang LineNumberingPushbackReader)))

(defn- ns-form? [form]
  (and (seq? form)
       (contains? '#{ns clojure.core/ns} (first form))))

(defn- branch
  "The branch of the reader conditional `rc` for `platform`, wrapped in a
  vector so that a nil branch differs from no branch."
  [rc platform]
  (some (fn [[feature form]]
          (when (contains? #{platform :default} feature)
            [form]))
        (partition 2 (:form rc))))

(defn for-platform
  "`form` with its reader conditionals replaced by their branches for
  `platform` (`:clj` or `:cljs`). A conditional without such a branch goes
  away, like it does when the reader reads it. Like the reader, this throws
  when a splicing conditional's branch isn't a list or a vector."
  [form platform]
  (letfn [(elements [coll]
            (mapcat (fn [x]
                      (if (reader-conditional? x)
                        (if-let [[picked] (branch x platform)]
                          (if (:splicing? x)
                            (if (sequential? picked)
                              (elements picked)
                              (throw (IllegalArgumentException.
                                      "Spliced form in a reader conditional must be a list or a vector")))
                            [(for-platform picked platform)])
                          [])
                        [(for-platform x platform)]))
                    coll))]
    (cond
      (reader-conditional? form) (when-let [[picked] (branch form platform)]
                                   (for-platform picked platform))
      (seq? form) (with-meta (apply list (elements form)) (meta form))
      (vector? form) (with-meta (vec (elements form)) (meta form))
      (set? form) (with-meta (set (elements form)) (meta form))
      (map? form) (with-meta (into {}
                                   (map (fn [[k v]]
                                          [(for-platform k platform)
                                           (for-platform v platform)]))
                                   form)
                    (meta form))
      :else form)))

(defn- read-until
  "Read the forms of `source` until `(pick form)` returns something, and
  return that. Reader conditionals stay in the forms."
  [source pick]
  (try
    (with-open [reader (LineNumberingPushbackReader. (io/reader source))]
      (binding [*read-eval* false
                ;; Only #inst and #uuid get read, by `default-data-readers`.
                *data-readers* {}
                *default-data-reader-fn* tagged-literal]
        (loop []
          (let [form (read {:read-cond :preserve, :eof ::eof} reader)]
            (when-not (identical? ::eof form)
              (or (pick form) (recur)))))))
    (catch Exception _)
    ;; from something nested very deep
    (catch StackOverflowError _)))

(defn read-ns-form
  "Read the first `ns` form from `source`, which can be anything
  `clojure.java.io/reader` accepts (a File, a URL, a path or a Reader).

  With `platform` (`:clj` or `:cljs`), reader conditionals get resolved for
  it, and this is the first `ns` form on that platform. Without `platform`,
  they stay in, so the form might be a conditional around `ns` forms, as in
  `#?(:clj (ns a ...) :cljs (ns a ...))`, for `for-platform` to resolve. A
  file with a separate conditional `ns` form for each platform needs
  `platform`, as only the first of those is found without it.

  Nothing gets evaluated: `#=` makes the form unreadable, and tagged literals
  come back as `clojure.lang.TaggedLiteral`s, except for `#inst` and `#uuid`.
  Returns nil when there's no `ns` form or `source` can't be read."
  ([source]
   (read-until source (fn [form]
                        (when (or (ns-form? form)
                                  (and (reader-conditional? form)
                                       (some ns-form? (take-nth 2 (rest (:form form))))))
                          form))))
  ([source platform]
   (read-until source (fn [form]
                        (let [form (for-platform form platform)]
                          (when (ns-form? form)
                            form))))))

(defn- clauses
  "The clauses of `ns-form` named like `clause-names`, e.g. `#{\"require\"}`.
  Like `ns` itself, this goes by the name, so `(require ...)` counts too."
  [ns-form clause-names]
  (filter (fn [clause]
            (and (sequential? clause)
                 (let [head (first clause)]
                   (and (or (keyword? head) (symbol? head))
                        (contains? clause-names (name head))))))
          (drop 2 ns-form)))

(defn- libspec
  "The map for the libspec `spec` in the clause named `clause`. `prefix`
  qualifies a name that came from a prefix list."
  [clause prefix spec]
  (let [qualify #(if prefix (symbol (str prefix "." %)) %)]
    (if (sequential? spec)
      (into {:ns (qualify (first spec)), :clause clause}
            (map vec)
            (partition 2 (rest spec)))
      {:ns (qualify spec), :clause clause})))

(defn- prefix-list? [spec]
  (and (sequential? spec)
       (symbol? (first spec))
       (next spec)
       (not (keyword? (second spec)))))

(defn libspecs
  "The libspecs of the `:require`, `:use`, `:require-macros`, `:use-macros` and
  `:require-global` clauses of `ns-form`, as maps like

    {:ns clojure.string, :as str, :refer [join], :clause :require}

  with prefix lists expanded. `:ns` is a symbol, or a string for a
  JavaScript module in ClojureScript. Other options come as they're written,
  so a libspec from `:use` doesn't get a `:refer`. Resolve reader
  conditionals first, with `for-platform`."
  [ns-form]
  (for [[head & specs] (clauses ns-form #{"require" "use" "require-macros" "use-macros"
                                          "require-global"})
        :let [clause (keyword (name head))]
        spec specs
        ;; flags like :reload
        :when (not (keyword? spec))
        libspec (if (prefix-list? spec)
                  (map #(libspec clause (first spec) %) (rest spec))
                  [(libspec clause nil spec)])]
    libspec))

(defn aliases
  "A map from each alias `ns-form` defines (with `:as` or `:as-alias`) to the
  namespace it stands for. What doesn't make sense as one, like a keyword for
  an alias in a libspec that's still being written, is left out."
  [ns-form]
  (into {}
        (comp (filter (comp (some-fn symbol? string?) :ns))
              (mapcat (fn [{:keys [ns as as-alias]}]
                        (for [alias [as as-alias]
                              :when (symbol? alias)]
                          [alias ns]))))
        (libspecs ns-form)))

(defn imports
  "The fully qualified names of the classes the `:import` clauses of `ns-form`
  import, as symbols."
  [ns-form]
  (for [[_ & specs] (clauses ns-form #{"import"})
        spec specs
        class-name (if (sequential? spec)
                     (map #(symbol (str (first spec) "." %)) (rest spec))
                     [spec])]
    class-name))
