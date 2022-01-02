(ns orchard.xref
  "Utilities for finding function dependencies and
  references."
  {:added "0.5"}
  (:require
   [clojure.tools.reader :as r]
   [clojure.tools.reader.reader-types :as rts]
   [clojure.repl :as repl]
   [orchard.query :as q]))

;;;;;;; taken from sayid, needs to be cleaned up
(defn- mk-dummy-whitespace
  [lines cols]
  (apply str
         (concat (repeat lines "\n")
                 (repeat cols " "))))

(defn- mk-positionalble-src-logging-push-back-rdr
  [s file line col]
  (rts/source-logging-push-back-reader (str (mk-dummy-whitespace (dec line) ;;this seem unfortunate
                                                                 (dec col))
                                            s)
                                       (+ (count s) line col 1)
                                       file))

(defn- hunt-down-source
  [fn-sym]
  (let [{:keys [source file line column]} (-> fn-sym
                                              resolve
                                              meta)]
    ;; (println file)
    (or source
        (and file (r/read (mk-positionalble-src-logging-push-back-rdr
                           (or
                            (clojure.repl/source-fn fn-sym)
                            (->> file
                                 slurp
                                 clojure.string/split-lines
                                 (drop (dec line))
                                 (clojure.string/join "\n"))
                            "nil")
                           file
                           line
                           column))))))

;;;;;;; taken from sayid, needs to be cleaned up

(defn- as-val
  "Convert `thing` to a function value."
  [thing]
  (cond
    (var? thing) (var-get thing)
    (symbol? thing) (var-get (find-var thing))
    (fn? thing) thing))

(defn- f->sym [f]
  (-> f .getClass .getName repl/demunge symbol))

(defn- fn-source [f]
  (hunt-down-source (f->sym f)))

(defn- fn-name [f]
  (-> f .getName repl/demunge symbol))

(defonce classbytes (atom {}))

(defn- recompile [ns-sym form]
  (push-thread-bindings
   {clojure.lang.Compiler/LOADER
    (proxy [clojure.lang.DynamicClassLoader] [@clojure.lang.Compiler/LOADER]
      (defineClass
        ([name bytes src]
         (swap! classbytes assoc name bytes)
         (proxy-super defineClass name bytes src))))})
  (try
    (let [line @clojure.lang.Compiler/LINE
          column @clojure.lang.Compiler/COLUMN
          line (if-let [line (:line (meta form))]
                 line
                 line)
          column (if-let [column (:column (meta form))]
                   column
                   column)]
      (push-thread-bindings {clojure.lang.Compiler/LINE line
                             clojure.lang.Compiler/COLUMN column})
      (try
        (let [form (macroexpand form)]
          (binding [*ns* (create-ns ns-sym)]
            (clojure.lang.Compiler/analyze
             clojure.lang.Compiler$C/EVAL
             (nth form 2))))
        (finally
          (pop-thread-bindings))))
    (finally
      (pop-thread-bindings))))



(defn- fn-deps-class
  [val]
  (let [v (if (class? val)
            val
            (eval val))]
    (set (some->> v .getDeclaredFields
                  (keep (fn [^java.lang.reflect.Field f]
                          (or (and (identical? clojure.lang.Var (.getType f))
                                   (java.lang.reflect.Modifier/isPublic (.getModifiers f))
                                   (java.lang.reflect.Modifier/isStatic (.getModifiers f))
                                   (-> f .getName (.startsWith "const__"))
                                   (.get f (fn-name v)))
                              nil)))))))

(defn fn-deps
  "Returns a set with all the functions invoked by `val`.
  `val` can be a function value, a var or a symbol."
  {:added "0.5"}
  [f]
  (reset! classbytes {})
  (let [f (as-val f)]
    (when-let [source (and (fn? f) (fn-source f))]
      (let [sym (f->sym f)
            b (do (recompile (-> sym namespace symbol) source) @classbytes)
            class-names (map first b)
            deps (set (mapcat #(-> % symbol fn-deps-class) class-names))]
        deps))))

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
        all-vars (q/vars {:ns-query {:project? true} :private? true})
        deps-map (zipmap all-vars (map fn-deps all-vars))]
    (map first (filter (fn [[_k v]] (contains? v var)) deps-map))))

(defn- dummy-fn [_x]
  (map #(* % 2) (filter even? (range 1 10))))

(comment

  (fn-deps #'user/jdk8?)
  (fn-deps #'orchard.meta/ns-file)
  (def vars (q/vars {:ns-query {:project? true} :private? true}))
  (map fn-deps vars)
  (drop 100 vars)
  (when-let [x (fn? (as-val user/jdk8?))]
    "some"))