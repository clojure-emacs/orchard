(ns orchard.trace
  "Faster and prettier reimplementation of `clojure.tools.trace` with unnecessary
  parts removed. Used for tracing function invocations and their results."
  (:require [clojure.string :as string]
            [orchard.print :as print]))

;;;; Internals

(defmacro ^:private appendn
  "Add all strings in `args` to the given StringBuilder `sb`."
  [sb & args]
  (let [sbsym (with-meta (gensym "sb") {:tag 'StringBuilder})]
    `(let [~sbsym ~sb]
       ~@(map (fn [x] `(.append ~sbsym ~x)) args))))

(defn- funcall-to-string
  "Print the invokation of a function and arguments to a string."
  ([fname args]
   (funcall-to-string fname args ""))
  ([fname, args, ^String outer-prefix]
   (let [max-args 20 ;; Most args we want to display before truncating.
         res (StringBuilder.)
         argn (bounded-count (inc max-args) args)
         more-than-max-args? (> argn max-args)
         argn (min argn max-args)]
     (appendn res outer-prefix "(" fname)
     (dotimes [i argn]
       (let [arg (nth args i)]
         (appendn res " " arg)))
     (when more-than-max-args?
       (.append res " ..."))
     (.append res ")")
     (.toString res))))

(def ^:private ^:dynamic *depth* 0)

(defn- trace-indent []
  (string/join (repeat *depth* "│ ")))

(defmacro ^:private limit-printing [& body]
  ;; Good defaults for orchard.print.
  `(binding [*print-length* 100
             *print-level* 5
             print/*max-atom-length* 150
             print/*max-total-length* 10000]
     ~@body))

(defn- trace-fn-call [name f args]
  ;; Good defaults for orchard.print.
  (limit-printing
   (println (trace-indent))
   (println
    (funcall-to-string (str name) (map print/print-str args)
                       (trace-indent))))
  (let [value (binding [*depth* (inc *depth*)]
                (apply f args))
        res-prefix "└─→ "]
    (limit-printing
     (binding [*depth* (inc *depth*)]
       (println (trace-indent)))
     (println (str (trace-indent) res-prefix (print/print-str value))))
    value))

(defn- resolve-var ^clojure.lang.Var [v]
  (if (var? v) v (resolve v)))

;;;; Public API

(def ^:private traced-vars (atom #{}))
(def ^:private traced-nses (atom #{}))

(defn traceable?
  "Return true if the given var can be traced."
  [v]
  (let [v (resolve-var v)]
    (and (ifn? @v) (not (:macro (meta v))))))

(defn traced?
  "Return true if the given var is currently traced."
  [v]
  (let [v (resolve-var v)]
    (contains? (meta v) ::traced)))

(defn trace-var*
  "If the specified Var holds an IFn and is not marked as a macro, its
  contents is replaced with a version wrapped in a tracing call;
  otherwise nothing happens. Can be undone with `untrace-var*`."
  [v]
  (let [v (resolve-var v)
        ns (.ns v)
        s  (.sym v)]
    (when (and (traceable? v) (not (traced? v)))
      (let [f @v
            vname (symbol (str ns "/" s))]
        (swap! traced-vars conj v)
        (alter-var-root v #(fn tracing-wrapper [& args]
                             (trace-fn-call vname % args)))
        (alter-meta! v assoc ::traced f)
        v))))

(defn untrace-var*
  "Reverses the effect of `trace-var*` for the given Var, replacing the traced
  function with the original, untraced version. No-op for non-traced Vars."
  [v]
  (let [v (resolve-var v)
        f (::traced (meta v))]
    (when f
      (alter-var-root v (constantly (::traced (meta v))))
      (alter-meta! v dissoc ::traced)
      (swap! traced-vars disj v)
      v)))

(defn trace-ns*
  "Trace all Vars in the given namespace. Can be undone with `untrace-ns*`. `ns`
  should be a namespace object or a symbol.

  No-op for clojure.core and orchard.trace."
  [ns]
  (let [ns (the-ns ns)]
    (when-not ('#{clojure.core orchard.trace} (.name ns))
      (->> (ns-interns ns)
           vals
           (filter (comp fn? var-get))
           (run! trace-var*))
      (swap! traced-nses conj ns))))

(defn untrace-ns*
  "Untrace all Vars in the given namespace."
  [ns]
  (let [ns (the-ns ns)]
    (->> (ns-interns ns)
         vals
         (filter (comp fn? var-get))
         (run! untrace-var*))
    (swap! traced-nses disj ns)))

(defn untrace-all
  "Reverses the effect of tracing for all already traced vars and namespaces."
  []
  (run! untrace-ns* @traced-nses)
  (run! untrace-var* @traced-vars))
