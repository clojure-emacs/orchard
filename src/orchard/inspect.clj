(ns orchard.inspect
  "Clojure data structure inspection functionality.
  This code has a long history and at various points of
  time it lived in different projects. Originally
  it was part of swank-clojure, afterwards it was moved to
  javert, then forked to another project from which it
  was contributed to cider-nrepl. Finally cider-nrepl
  was split into two libraries and the code ended up here.

  Pretty wild, right?"
  (:require
   [clojure.string :as s]
   [orchard.misc :as misc])
  (:import
   clojure.lang.Seqable
   (java.lang.reflect Constructor Field Method Modifier)
   (java.util List Map)))

;; Datafy and Nav are only available since Clojure 1.10
(require 'clojure.core.protocols)

(def ^:private datafy
  (misc/call-when-resolved 'clojure.core.protocols/datafy))

(def ^:private nav
  (misc/call-when-resolved 'clojure.core.protocols/nav))

;;
;; Navigating Inspector State
;;

(declare inspect-render inspect-value)

(defn- reset-index [inspector]
  (merge inspector {:counter 0 :index []}))

(defn push-item-to-path
  "Takes the current inspector index, the `idx` of the value in it to be navigated
  to, and the path so far, and returns the updated path to the selected value."
  [index idx path current-page page-size]
  (if (>= idx (count index))
    (conj path '<unknown>)
    (if (= idx 0)
      (conj path 'class)
      (let [klass (first index)]
        (cond
          ;; If value's class is a map, going down means jumping into either key
          ;; or value.
          ((supers klass) clojure.lang.IPersistentMap)
          (if (even? idx)
            ;; Even index means jumping into the value by the key.
            (let [key (nth index (dec idx))]
              (conj path (if (keyword? key)
                           key
                           (list 'get key))))
            ;; Odd index means finding the map entry and taking its key
            (conj path (list 'find (nth index idx)) 'key))

          ;; For sequential things going down means getting the nth value.
          ((supers klass) clojure.lang.Sequential)
          (let [coll-idx (+ (* (or current-page 0) page-size)
                            (dec idx))]
            (conj path (list 'nth coll-idx)))

          :else (conj path '<unknown>))))))

(defn pop-item-from-path
  "Takes the current inspector path, and returns an updated path one level up."
  [path]
  (let [last-node (peek path)]
    (if (= last-node 'key)
      (pop (pop path)) ; pop twice to remove <(find :some-key) key>
      (pop path))))

(defn clear
  "Clear an inspector's state."
  [inspector]
  (merge (reset-index inspector)
         {:value nil, :stack [], :path [], :pages-stack [],
          :current-page 0, :rendered '(), :indentation 0}))

(defn fresh
  "Return an empty inspector."
  []
  (inspect-render (clear {:page-size 32})))

(defn start
  "Put a new value onto the inspector stack."
  [inspector value]
  (-> (clear inspector)
      (inspect-render value)))

(defn up
  "Pop the stack and re-render an earlier value."
  [inspector]
  (let [{:keys [stack pages-stack]} inspector]
    (if (empty? stack)
      (inspect-render inspector)
      (-> inspector
          (update-in [:path] pop-item-from-path)
          (assoc :current-page (peek pages-stack))
          (update-in [:pages-stack] pop)
          (inspect-render (last stack))
          (update-in [:stack] pop)))))

(defn down
  "Drill down to an indexed object referred to by the previously
   rendered value."
  [inspector ^Integer idx]
  {:pre [(integer? idx)]}
  (let [{:keys [index path current-page page-size]} inspector
        new (get index idx)
        val (:value inspector)
        new-path (push-item-to-path index idx path current-page page-size)]
    (-> (update-in inspector [:stack] conj val)
        (update-in [:pages-stack] conj current-page)
        (assoc :current-page 0)
        (assoc :path new-path)
        (inspect-render new))))

(defn next-page
  "Jump to the next page when inspecting a paginated sequence/map. Does nothing
  if already on the last page."
  [inspector]
  (inspect-render (update-in inspector [:current-page] inc)))

(defn prev-page
  "Jump to the previous page when inspecting a paginated sequence/map. Does
  nothing if already on the first page."
  [inspector]
  (inspect-render (update-in inspector [:current-page] dec)))

(defn set-page-size
  "Set the page size in pagination mode to the specified value. Current page
  will be reset to zero."
  [inspector new-page-size]
  {:pre [(integer? new-page-size) (pos? new-page-size)]}
  (inspect-render (assoc inspector
                         :page-size new-page-size
                         :current-page 0)))

(defn set-max-atom-length
  "Set the maximum length of atomic collection members before they're truncated."
  [inspector max-atom-length]
  {:pre [(integer? max-atom-length)]}
  (inspect-render (assoc inspector :max-atom-length max-atom-length)))

(defn set-max-coll-size
  "Set the maximum number of nested collection members to print before truncating."
  [inspector max-coll-size]
  {:pre [(integer? max-coll-size)]}
  (inspect-render (assoc inspector :max-coll-size max-coll-size)))

(defn eval-and-inspect
  "Evaluate the given expression where `v` is bound to the currently inspected
  value. Open the evaluation result in the inspector."
  [inspector expr]
  (let [{:keys [current-page value]} inspector
        eval-fn `(fn [~'v] ~(read-string expr))
        result ((eval eval-fn) value)]
    (-> (update inspector :stack conj value)
        (update :pages-stack conj current-page)
        (assoc :current-page 0)
        (update :path conj '<unknown>)
        (inspect-render result))))

(defn def-current-value
  "Define the currently inspected value as a var with the given name in the
  provided namespace."
  [inspector namespace var-name]
  (intern namespace (symbol var-name) (:value inspector))
  (inspect-render inspector))

(declare inspector-value-string)

;;
;; Render values onto the inspector's current state
;;
;; Good for method extenders to use

(defn- atom? [val]
  (some #(% val) [number? string? symbol? keyword?]))

(defn safe-pr-seq
  ([value fmt]
   (safe-pr-seq value " " fmt))
  ([value sep fmt]
   (->> (map inspect-value value)
        (s/join sep)
        (format fmt))))

(def ^:private ^:dynamic *max-atom-length* 150)
(def ^:private ^:dynamic *max-coll-size* 5)

(defn- short? [coll]
  ;; Prefer `bounded-count` if available (clojure 1.9+) or fall back to `count`.
  (let [len (if-let [;; NOTE can't name this `bounded-count` because eastwood's
                     ;; :local-shadows-var warning can't be suppressed.
                     bounded-count-fn (some-> (resolve 'clojure.core/bounded-count)
                                              (var-get))]
              (bounded-count-fn (inc *max-coll-size*) coll)
              (count coll))]
    (<= len *max-coll-size*)))

(defn- truncate-string [s]
  (when s
    (let [len (count s)]
      (if (> len *max-atom-length*)
        (str (subs s 0 (max (- *max-atom-length* 3) 0)) "...")
        s))))

(defn value-types [value]
  (cond
    (nil? value)                                   nil
    (atom? value)                                  :atom
    (and (instance? Seqable value) (empty? value)) :seq-empty
    (and (map? value) (short? value))              :map
    (map-entry? value)                             :map-entry
    (map? value)                                   :map-long
    (and (vector? value) (short? value))           :vector
    (vector? value)                                :vector-long
    (misc/lazy-seq? value)                         :lazy-seq
    (and (seq? value) (short? value))              :list
    (seq? value)                                   :list-long
    (and (set? value) (short? value))              :set
    (set? value)                                   :set-long
    (and (instance? List value) (short? value))    :list
    (instance? List value)                         :list-long
    (and (instance? Map value) (short? value))     :map
    (instance? Map value)                          :map-long
    (and (.isArray (class value)) (short? value))  :array
    (.isArray (class value))                       :array-long
    :else (or (:inspector-tag (meta value))
              (type value))))

(defmulti inspect-value #'value-types)

(defmethod inspect-value nil [_value]
  "nil")

(defmethod inspect-value :atom [value]
  (truncate-string (pr-str value)))

(defmethod inspect-value :map-entry [[k v]]
  (str (inspect-value k) " " (inspect-value v)))

(defmethod inspect-value :seq-empty [value]
  (pr-str value))

(defmethod inspect-value :map [value]
  (safe-pr-seq value ", " "{ %s }"))

(defmethod inspect-value :map-long [value]
  (safe-pr-seq (take *max-coll-size* value) ", " "{ %s, ... }"))

(defmethod inspect-value :vector [value]
  (safe-pr-seq value "[ %s ]"))

(defmethod inspect-value :vector-long [value]
  (safe-pr-seq (take *max-coll-size* value) "[ %s ... ]"))

(defmethod inspect-value :lazy-seq [value]
  (let [prefix-length (inc *max-coll-size*)
        prefix (take prefix-length value)]
    (if (= (count prefix) prefix-length)
      (safe-pr-seq (take *max-coll-size* value) "( %s ... )")
      (safe-pr-seq prefix "( %s )"))))

(defmethod inspect-value :list [value]
  (safe-pr-seq value "( %s )"))

(defmethod inspect-value :list-long [value]
  (safe-pr-seq (take *max-coll-size* value) "( %s ... )"))

(defmethod inspect-value :set [value]
  (safe-pr-seq value "#{ %s }"))

(defmethod inspect-value :set-long [value]
  (safe-pr-seq (take *max-coll-size* value) "#{ %s ... }"))

(defmethod inspect-value :array [value]
  (let [ct (.getName (or (.getComponentType (class value)) Object))]
    (safe-pr-seq value ", " (str ct "[] { %s }"))))

(defmethod inspect-value :array-long [value]
  (let [ct (.getName (or (.getComponentType (class value)) Object))]
    (safe-pr-seq (take *max-coll-size* value) ", " (str ct "[] { %s, ... }"))))
(defmethod inspect-value java.lang.Class [value]
  (pr-str value))

(defmethod inspect-value :default [value]
  (truncate-string (str value)))

(defn render-onto [inspector coll]
  (update-in inspector [:rendered] concat coll))

(defn render [inspector & values]
  (render-onto inspector values))

(defn render-ln [inspector & values]
  (render-onto inspector (concat values '((:newline)))))

(defn- indent [inspector]
  (update inspector :indentation + 2))

(defn- unindent [inspector]
  (update inspector :indentation - 2))

(defn- padding [{:keys [indentation]}]
  (when (and (number? indentation) (pos? indentation))
    (apply str (repeat indentation " "))))

(defn- render-indent [inspector & values]
  (let [padding (padding inspector)]
    (cond-> inspector
      padding
      (render padding)
      (seq values)
      (render-onto values))))

(defn- render-section-header [inspector section]
  (-> (render-ln inspector)
      (render (format "%s--- %s:" (or (padding inspector) "") (name section)))
      (render-ln)))

(defn render-value [inspector value]
  (let [{:keys [counter]} inspector
        expr `(:value ~(inspect-value value) ~counter)]
    (-> inspector
        (update-in [:index] conj value)
        (update-in [:counter] inc)
        (update-in [:rendered] concat (list expr)))))

(defn render-labeled-value [inspector label value]
  (-> inspector
      (render-indent label ": ")
      (render-value value)
      (render-ln)))

(defn- render-class-name [inspector obj]
  (render-labeled-value inspector "Class" (class obj)))

(defn render-map-values [inspector mappable]
  (reduce (fn [ins [key val]]
            (-> ins
                (render-indent)
                (render-value key)
                (render " = ")
                (render-value val)
                (render-ln)))
          inspector
          mappable))

(defn render-indexed-values
  ([inspector obj] (render-indexed-values inspector obj 0))
  ([inspector obj idx-starts-from]
   (loop [ins inspector, obj (seq obj), idx idx-starts-from]
     (if obj
       (recur (-> ins
                  (render-indent (str idx) ". ")
                  (render-value (first obj))
                  (render-ln))
              (next obj) (inc idx))
       ins))))

(defn- last-page [{:keys [current-page page-size]} obj]
  (if (or (instance? clojure.lang.Counted obj)
          ;; if there are no more items after the current page,
          ;; we must have reached the end of the collection, so
          ;; it's not infinite.
          (empty? (drop (* (inc current-page) page-size) obj)))
    (quot (dec (count obj)) page-size)
    ;; possibly infinite
    Integer/MAX_VALUE))

(defn- render-page-info [{:keys [current-page page-size] :as inspector} obj]
  (if-not (sequential? obj)
    inspector
    (let [last-page (last-page inspector obj)
          paginate? (not= last-page 0)]
      (if-not paginate?
        inspector
        (-> (render-section-header inspector "Page Info")
            (indent)
            (render-indent (format "Page size: %d, showing page: %d of %s"
                                   page-size (inc current-page)
                                   (if (= last-page Integer/MAX_VALUE)
                                     "?" (inc last-page))))
            (unindent))))))

(defn- current-page [{:keys [current-page] :as inspector} obj]
  (let [last-page (last-page inspector obj)]
    ;; current-page might contain an incorrect value, fix that
    (cond (< current-page 0) 0
          (> current-page last-page) last-page
          :else current-page)))

(defn- chunk-to-display [{:keys [page-size] :as inspector} obj]
  (let [start-idx (* (current-page inspector obj) page-size)]
    (->> obj (drop start-idx) (take page-size))))

(defn render-collection-paged
  "Render a single page of either an indexed or associative collection."
  [inspector obj]
  (let [{:keys [page-size]} inspector
        last-page (last-page inspector obj)
        current-page (current-page inspector obj)
        start-idx (* current-page page-size)
        chunk-to-display (chunk-to-display inspector obj)
        paginate? (not= last-page 0)]
    (as-> inspector ins
      (if (> current-page 0)
        (-> ins
            (render-indent "...")
            (render-ln))
        ins)

      (if (or (map? obj) (instance? Map obj))
        (render-map-values ins chunk-to-display)
        (render-indexed-values ins chunk-to-display start-idx))

      (if (< current-page last-page)
        (-> (render-indent ins "...")
            (render-ln))
        ins)

      (if paginate?
        (assoc ins :current-page current-page)
        ins))))

(defn render-meta-information [inspector obj]
  (if (seq (meta obj))
    (-> inspector
        (render-section-header "Meta Information")
        (indent)
        (render-map-values (meta obj))
        (unindent))
    inspector))

(defn- nav-datafy-tx [obj]
  (comp (map (fn [[k v]] (some->> (nav obj k v) datafy (vector k)))) (remove nil?)))

(defn- nav-datafy [obj]
  (let [data (datafy obj)]
    (cond (map? data)
          (into {} (nav-datafy-tx obj) data)
          (or (sequential? data) (set? data))
          (map datafy data))))

(defn- render-datafy? [inspector obj]
  (cond (not misc/datafy?)
        false
        (map? obj)
        (not= obj (nav-datafy obj))
        (or (sequential? obj) (set? obj))
        (not= (chunk-to-display inspector obj)
              (map datafy (chunk-to-display inspector obj)))
        :else (not= obj (datafy obj))))

(declare inspect)

(defn- render-datafy-content [inspector obj]
  (let [contents (nav-datafy obj)]
    (cond (map? contents)
          (render-collection-paged inspector contents)
          (sequential? contents)
          (render-collection-paged inspector contents)
          :else (-> (indent inspector)
                    (inspect contents)
                    (unindent)))))

(defn- render-datafy [inspector obj]
  ;; Only render the datafy section if the datafyed version of the object is
  ;; different than object, since we don't want to show the same data twice to
  ;; the user.
  (if-not (render-datafy? inspector obj)
    inspector
    (-> (render-section-header inspector "Datafy")
        (indent)
        (render-datafy-content obj)
        (unindent))))

;; Inspector multimethod
(defn known-types [_ins obj]
  (cond
    (nil? obj) :nil
    (map? obj) :coll
    (vector? obj) :coll
    (seq? obj) :coll
    (set? obj) :coll
    (var? obj) :var
    (string? obj) :string
    (instance? Class obj) :class
    (instance? clojure.lang.Namespace obj) :namespace
    (instance? clojure.lang.ARef obj) :aref
    (instance? List obj) :coll
    (instance? Map obj) :coll
    (.isArray (class obj)) :array
    :else (or (:inspector-tag (meta obj))
              (type obj))))

(defmulti inspect #'known-types)

(defmethod inspect :nil [inspector _obj]
  (-> inspector
      (render-ln "nil")))

(defmethod inspect :coll [inspector obj]
  (-> (render-class-name inspector obj)
      (render-meta-information obj)
      (render-section-header "Contents")
      (indent)
      (render-collection-paged obj)
      (unindent)
      (render-datafy obj)))

(defmethod inspect :array [inspector obj]
  (-> (render-class-name inspector obj)
      (render-labeled-value "Count" (java.lang.reflect.Array/getLength obj)) ; avoid reflection warning from Clojure compiler
      (render-labeled-value "Component Type" (.getComponentType (class obj)))
      (render-section-header "Contents")
      (indent)
      (render-collection-paged obj)
      (unindent)
      (render-datafy obj)))

(defn- render-var-value [inspector ^clojure.lang.Var obj]
  (if-not (.isBound obj)
    inspector
    (-> (render-indent inspector "Value: ")
        (render-value (var-get obj))
        (render-ln))))

(defmethod inspect :var [inspector ^clojure.lang.Var obj]
  (-> (render-class-name inspector obj)
      (render-var-value obj)
      (render-meta-information obj)
      (render-datafy obj)))

(defn- render-indent-str-lines [inspector s]
  (reduce #(-> (render-indent %1 (str %2))
               (render-ln))
          inspector (s/split-lines s)))

(defmethod inspect :string [inspector ^java.lang.String obj]
  (-> (render-class-name inspector obj)
      (render "Value: " (pr-str obj))
      (render-ln)
      (render-section-header "Print")
      (indent)
      (render-indent-str-lines obj)
      (unindent)))

(defmethod inspect :default [inspector obj]
  (let [class-chain (loop [c (class obj), res ()]
                      (if c
                        (recur (.getSuperclass c) (cons c res))
                        res))
        all-fields (mapcat #(.getDeclaredFields ^Class %) class-chain)

        {static true, non-static false}
        (group-by #(Modifier/isStatic (.getModifiers ^Field %)) all-fields)]
    (letfn [(field-name [^Field f]
              (.getName f))

            (field-val [^Field f]
              (let [^Exception e
                    (try (.setAccessible f true)
                         nil
                         (catch Exception e
                           ;; We want to handle specifically SecurityException
                           ;; and j.l.r.InaccessibleObjectException, but the
                           ;; latter only comes with Java9+, so let's just
                           ;; catch everything instead.
                           e))]
                (try (.get f obj)
                     (catch java.lang.IllegalAccessException _
                       (symbol
                        (format "<Access denied%s>"
                                (when e (str " (" (.getName (.getClass e)) ")"))))))))

            (render-fields [inspector section-name fields]
              (if (seq fields)
                (-> inspector
                    (render-section-header section-name)
                    (indent)
                    (render-map-values (->> fields
                                            (map (fn [f] [(field-name f) (field-val f)]))
                                            (into (sorted-map))))
                    (unindent))
                inspector))]
      (-> inspector
          (render-labeled-value "Class" (class obj))
          (render-labeled-value "Value" (pr-str obj))
          (render-fields "Fields" non-static)
          (render-fields "Static fields" static)
          (render-datafy obj)))))

(defn- render-section [obj inspector [section sort-key-fn]]
  (let [method (symbol (str ".get" (name section)))
        elements (eval (list method obj))]
    (if-not (seq elements)
      inspector
      (unindent (reduce (fn [ins elt]
                          (-> ins
                              (render-indent)
                              (render-value elt)
                              (render-ln)))
                        (-> inspector
                            (render-section-header section)
                            (indent))
                        (sort-by sort-key-fn elements))))))

(defmethod inspect :class [inspector ^Class obj]
  (-> (reduce (partial render-section obj)
              (render-class-name inspector obj)
              [[:Interfaces #(.getName ^Class %)]
               [:Constructors #(.toGenericString ^Constructor %)]
               [:Fields #(.getName ^Field %)]
               [:Methods #(vector (.getName ^Method %) (.toGenericString ^Method %))]])
      (render-datafy obj)))

(defmethod inspect :aref [inspector ^clojure.lang.ARef obj]
  (-> (render-class-name inspector obj)
      (render-section-header "Contains")
      (indent)
      (inspect (deref obj))
      (unindent)
      (render-datafy obj)))

(defn ns-refers-by-ns [^clojure.lang.Namespace ns]
  (group-by (fn [^clojure.lang.Var v] (.ns v))
            (map val (ns-refers ns))))

(defn- render-ns-refers [inspector obj]
  (let [refers (ns-refers-by-ns obj)]
    (if-not (seq refers)
      inspector
      (-> (render-section-header inspector "Refer from")
          (indent)
          (render-map-values refers)
          (unindent)))))

(defn- render-ns-imports [inspector obj]
  (let [imports (ns-imports obj)]
    (if-not (seq imports)
      inspector
      (-> (render-section-header inspector "Imports")
          (indent)
          (render-indent)
          (render-value imports)
          (unindent)
          (render-ln)))))

(defn- render-ns-interns [inspector obj]
  (let [interns (ns-interns obj)]
    (if-not (seq interns)
      inspector
      (-> (render-section-header inspector "Interns")
          (indent)
          (render-indent)
          (render-value interns)
          (unindent)
          (render-ln)))))

(defmethod inspect :namespace [inspector ^clojure.lang.Namespace obj]
  (-> (render-class-name inspector obj)
      (render-labeled-value "Count" (count (ns-map obj)))
      (render-ns-refers obj)
      (render-ns-imports obj)
      (render-ns-interns obj)
      (render-datafy obj)))

;;
;; Entry point to inspect a value and get the serialized rep
;;
(defn render-reference [inspector]
  (let [{:keys [type ns sym expr]} (:reference inspector)]
    (cond (= type :var)
          (render-ln inspector "Var: #'" ns "/" sym)
          (= type :expr)
          (render-ln inspector "Expr: " expr)
          :else
          inspector)))

(defn render-path [inspector]
  (let [path (:path inspector)]
    (if (and (seq path) (not-any? #(= % '<unknown>) path))
      (-> (render-section-header inspector "Path")
          (indent)
          (render-indent (s/join " " (:path inspector)))
          (unindent))
      inspector)))

(defn inspect-render
  ([inspector] (inspect-render inspector (:value inspector)))
  ([inspector value]
   (binding [*max-atom-length* (or (:max-atom-length inspector) *max-atom-length*)
             *max-coll-size* (or (:max-coll-size inspector) *max-coll-size*)]
     (-> (reset-index inspector)
         (assoc :rendered [])
         (assoc :value value)
         (render-reference)
         (inspect value)
         (render-page-info value)
         (render-path)))))

;; Get a human readable printout of rendered sequence
(defmulti inspect-print-component first)

(defmethod inspect-print-component :newline [_]
  (prn))

(defmethod inspect-print-component :value [[_ & xs]]
  (print (str (first xs))))

(defmethod inspect-print-component :default [x]
  (print x))

(defn inspect-print [x]
  (print
   (with-out-str
     (doseq [component (:rendered (inspect-render (assoc (fresh) :display-fields true) x))]
       (inspect-print-component component)))))
