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
   [clojure.string :as s])
  (:import
   (java.lang.reflect Field)
   (java.util List Map)
   clojure.lang.Seqable))

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
          :current-page 0, :rendered '()}))

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

(defn eval-and-inspect
  "Evaluate the given expression where `v` is bound to the currently inspected
  value. Open the evaluation result in the inspector."
  [inspector expr]
  (let [{:keys [index path current-page page-size value]} inspector
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

(defn- short? [coll]
  (<= (count coll) 5))

(def ^:private truncate-max-length 150)

(defn- truncate-string [s]
  (when s
    (let [len (count s)]
      (if (> len truncate-max-length)
        (str (subs s 0 (- truncate-max-length 2)) "...")
        s))))

(defn value-types [value]
  (cond
    (nil? value)                                   nil
    (atom? value)                                  :atom
    (and (instance? Seqable value) (empty? value)) :seq-empty
    (and (map? value) (short? value))              :map
    (map? value)                                   :map-long
    (and (vector? value) (short? value))           :vector
    (vector? value)                                :vector-long
    (and (seq? value) (not (counted? value)))      :lazy-seq
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

(defmethod inspect-value :atom [value]
  (truncate-string (pr-str value)))

(defmethod inspect-value :seq-empty [value]
  (pr-str value))

(defmethod inspect-value :map [value]
  (->> value
       (map (fn [[k v]]
              (str (inspect-value k) " " (inspect-value v))))
       (s/join ", ")
       (format "{ %s }")))

(defmethod inspect-value :map-long [value]
  (let [[k v] (first value)]
    (str "{ " (inspect-value k) " " (inspect-value v) ", ... }")))

(defmethod inspect-value :vector [value]
  (safe-pr-seq value "[ %s ]"))

(defmethod inspect-value :vector-long [value]
  (safe-pr-seq (take 5 value) "[ %s ... ]"))

(defmethod inspect-value :lazy-seq [value]
  (let [first-six (take 6 value)]
    (if (= (count first-six) 6)
      (safe-pr-seq (take 5 value) "( %s ... )")
      (safe-pr-seq first-six "( %s )"))))

(defmethod inspect-value :list [value]
  (safe-pr-seq value "( %s )"))

(defmethod inspect-value :list-long [value]
  (safe-pr-seq (take 5 value) "( %s ... )"))

(defmethod inspect-value :set [value]
  (safe-pr-seq value "#{ %s }"))

(defmethod inspect-value :set-long [value]
  (safe-pr-seq (take 5 value) "#{ %s ... }"))

(defmethod inspect-value :array [value]
  (let [ct (.getName (or (.getComponentType (class value)) Object))]
    (safe-pr-seq value ", " (str ct "[] { %s }"))))

(defmethod inspect-value :array-long [value]
  (let [ct (.getName (or (.getComponentType (class value)) Object))]
    (safe-pr-seq (take 5 value) ", " (str ct "[] { %s ... }"))))
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

(defn render-value [inspector value]
  (let [{:keys [counter]} inspector
        expr `(:value ~(inspect-value value) ~counter)]
    (-> inspector
        (update-in [:index] conj value)
        (update-in [:counter] inc)
        (update-in [:rendered] concat (list expr)))))

(defn render-labeled-value [inspector label value]
  (-> inspector
      (render label ": ")
      (render-value value)
      (render-ln)))

(defn render-map-values [inspector mappable]
  (reduce (fn [ins [key val]]
            (-> ins
                (render "  ")
                (render-value key)
                (render " = ")
                (render-value val)
                (render '(:newline))))
          inspector
          mappable))

(defn render-indexed-values
  ([inspector obj] (render-indexed-values inspector obj 0))
  ([inspector obj idx-starts-from]
   (loop [ins inspector, obj (seq obj), idx idx-starts-from]
     (if obj
       (recur (-> ins
                  (render "  " (str idx) ". ")
                  (render-value (first obj))
                  (render '(:newline)))
              (next obj) (inc idx))
       ins))))

(defn render-collection-paged
  "Render a single page of either an indexed or associative collection."
  [inspector obj]
  (let [{:keys [current-page page-size]} inspector
        last-page (if (or (instance? clojure.lang.Counted obj)
                          ;; if there are no more items after the current page,
                          ;; we must have reached the end of the collection, so
                          ;; it's not infinite.
                          (empty? (drop (* (inc current-page) page-size) obj)))
                    (quot (dec (count obj)) page-size)
                    Integer/MAX_VALUE) ;; possibly infinite
        ;; current-page might contain an incorrect value, fix that
        current-page (cond (< current-page 0) 0
                           (> current-page last-page) last-page
                           :else current-page)
        start-idx (* current-page page-size)
        chunk-to-display (->> obj
                              (drop start-idx)
                              (take page-size))
        paginate? (not= last-page 0)]
    (as-> inspector ins
      (if (> current-page 0)
        (-> ins
            (render "  ...")
            (render '(:newline)))
        ins)

      (if (or (map? obj) (instance? Map obj))
        (render-map-values ins chunk-to-display)
        (render-indexed-values ins chunk-to-display start-idx))

      (if (< current-page last-page)
        (render ins "  ...")
        ins)

      (if paginate?
        (-> ins
            (render '(:newline))
            (render (format "  Page size: %d, showing page: %d of %s"
                            page-size (inc current-page)
                            (if (= last-page Integer/MAX_VALUE)
                              "?" (inc last-page))))
            (assoc :current-page current-page))
        ins))))

(defn render-meta-information [inspector obj]
  (if (seq (meta obj))
    (-> inspector
        (render-ln "Meta Information: ")
        (render-map-values (meta obj)))
    inspector))

;; Inspector multimethod
(defn known-types [ins obj]
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
    :default (or (:inspector-tag (meta obj))
                 (type obj))))

(defmulti inspect #'known-types)

(defmethod inspect :nil [inspector obj]
  (-> inspector
      (render-ln "nil")))

(defmethod inspect :coll [inspector obj]
  (-> inspector
      (render-labeled-value "Class" (class obj))
      (render-meta-information obj)
      (render-ln "Contents: ")
      (render-collection-paged obj)))

(defmethod inspect :array [inspector obj]
  (-> inspector
      (render-labeled-value "Class" (class obj))
      (render-labeled-value "Count" (alength obj))
      (render-labeled-value "Component Type" (.getComponentType (class obj)))
      (render-ln "Contents: ")
      (render-collection-paged obj)))

(defmethod inspect :var [inspector ^clojure.lang.Var obj]
  (let [header-added
        (-> inspector
            (render-labeled-value "Class" (class obj))
            (render-meta-information obj))]
    (if (.isBound obj)
      (-> header-added
          (render "Value: ")
          (render-value (var-get obj)))
      header-added)))

(defmethod inspect :string [inspector ^java.lang.String obj]
  (-> inspector
      (render-labeled-value "Class" (class obj))
      (render "Value: " (pr-str obj))))

(defmethod inspect :default [inspector obj]
  (let [^"[Ljava.lang.reflect.Field;" fields (.getDeclaredFields (class obj))
        names (map #(.getName ^Field %) fields)
        get (fn [^Field f]
              (try (.setAccessible f true)
                   (catch java.lang.SecurityException e))
              (try (.get f obj)
                   (catch java.lang.IllegalAccessException e
                     "Access denied.")))
        vals (map get fields)]
    (-> inspector
        (render-labeled-value "Type" (class obj))
        (render-labeled-value "Value" (pr-str obj))
        (render-ln "---")
        (render-ln "Fields: ")
        (render-map-values (zipmap names vals)))))

(defn- render-class-section [inspector obj section]
  (let [method (symbol (str ".get" (name section)))
        elements (eval (list method obj))]
    (if (seq elements)
      `(~(name section) ": " (:newline)
                        ~@(mapcat (fn [f]
                                    `("  " (:value ~f) (:newline))) elements)))))

(defn- render-section [obj inspector section]
  (let [method (symbol (str ".get" (name section)))
        elements (eval (list method obj))]
    (if-not elements
      inspector
      (reduce (fn [ins elt]
                (-> ins
                    (render "  ")
                    (render-value elt)
                    (render-ln)))
              (-> inspector
                  (render-ln)
                  (render-ln "--- " (name section) ": "))
              elements))))

(defmethod inspect :class [inspector ^Class obj]
  (reduce (partial render-section obj)
          (render-labeled-value inspector "Type" (class obj))
          [:Interfaces :Constructors :Fields :Methods]))

(defmethod inspect :aref [inspector ^clojure.lang.ARef obj]
  (-> inspector
      (render-labeled-value "Type" (class obj))
      (render-ln "Contains:")
      (render-ln)
      (inspect (deref obj))))

(defn ns-refers-by-ns [^clojure.lang.Namespace ns]
  (group-by (fn [^clojure.lang.Var v] (.ns v))
            (map val (ns-refers ns))))

(defmethod inspect :namespace [inspector ^clojure.lang.Namespace obj]
  (-> inspector
      (render-labeled-value "Class" (class obj))
      (render-labeled-value "Count" (count (ns-map obj)))
      (render-ln "---")
      (render-ln "Refer from: ")
      (render-map-values (ns-refers-by-ns obj))
      (render-labeled-value "Imports" (ns-imports obj))
      (render-labeled-value "Interns" (ns-interns obj))))

;;
;; Entry point to inspect a value and get the serialized rep
;;
(defn render-reference [inspector]
  (let [{:keys [type ns sym expr]} (:reference inspector)]
    (cond (= type :var)
          (render-ln inspector "Var: #'" ns "/" sym)
          (= type :expr)
          (render-ln inspector "Expr: " expr)
          :default
          inspector)))

(defn render-path [inspector]
  (let [path (:path inspector)]
    (if (and (seq path) (not-any? #(= % '<unknown>) path))
      (-> inspector
          (render '(:newline))
          (render (str "  Path: "
                       (s/join " " (:path inspector)))))
      inspector)))

(defn inspect-render
  ([inspector] (inspect-render inspector (:value inspector)))
  ([inspector value] (-> (reset-index inspector)
                         (assoc :rendered [])
                         (assoc :value value)
                         (render-reference)
                         (inspect value)
                         (render-path))))

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
