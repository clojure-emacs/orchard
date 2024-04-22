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
   [clojure.string :as string]
   [orchard.misc :as misc]
   [orchard.print :as print])
  (:import
   (java.lang.reflect Constructor Field Method Modifier)
   (java.util List Map)))

;; Datafy Nav and tap> are only available since Clojure 1.10
(require 'clojure.core.protocols)

(def ^:private datafy
  (misc/call-when-resolved 'clojure.core.protocols/datafy))

(def ^:private nav
  (misc/call-when-resolved 'clojure.core.protocols/nav))

(def ^:private maybe-tap>
  (misc/call-when-resolved 'clojure.core/tap>))
;;
;; Navigating Inspector State
;;

(declare inspect-render)

(defn push-item-to-path
  "Takes the current inspector index, the `idx` of the value in it to be navigated
  to, and the path so far, and returns the updated path to the selected value."
  [index idx path current-page page-size]
  (if (>= idx (count index))
    (conj path '<unknown>)
    (if (= idx 0)
      (conj path 'class)
      (let [klass (first index)
            klass (if (class? klass)
                    klass
                    (-> klass str Class/forName))]
        (cond
          ;; If value's class is a map, going down means jumping into either key
          ;; or value.
          (.isAssignableFrom Map klass)
          (if (even? idx)
            ;; Even index means jumping into the value by the key.
            (let [key (nth index (dec idx))]
              (conj path (if (keyword? key)
                           key
                           (list 'get key))))
            ;; Odd index means finding the map entry and taking its key
            (conj path (list 'find (nth index idx)) 'key))

          ;; For sequential things going down means getting the nth value.
          (.isAssignableFrom List klass)
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

(def ^:private default-inspector-config
  "Default configuration values for the inspector."
  {:page-size        32    ; = Clojure's default chunked sequences chunk size.
   :max-atom-length  150
   :max-value-length 50000 ; Only to avoid printing graphs with loops.
   :max-coll-size    5})

(defn- reset-render-state [inspector]
  (assoc inspector :counter 0, :index [], :indentation 0, :rendered []))

(defn start
  "Create a new inspector for the `value`. Optinally accepts a `config` map (which
  can be an existing inspector with changed config)."
  ([value] (start {} value))
  ([config value]
   (-> default-inspector-config
       (merge (select-keys config (keys default-inspector-config)))
       (assoc :stack [], :path [], :pages-stack [], :current-page 0)
       (inspect-render value))))

(defn ^:deprecated clear
  "If necessary, use `(start inspector nil) instead.`"
  [inspector]
  (start inspector nil))

(defn ^:deprecated fresh
  "If necessary, use `(start nil)` instead."
  []
  (start nil))

(defn- items-on-page
  "Number of individual top level items on a single page."
  [{:keys [value page-size] :as _inspector}]
  (if (map? value)
    (* 2 page-size) ;; keys and values are treated as separate items
    page-size))

(defn- total-items
  "Total number of individual top level items in the current inspectable object."
  [{:keys [value] :as _inspector}]
  (cond
    (map? value)
    (* 2 (count value)) ;; keys and values are treated as separate items
    (instance? clojure.lang.Counted value)
    (count value)
    :else ;; possibly infinite
    Integer/MAX_VALUE))

(defn- last-page
  ([inspector] (last-page inspector (:value inspector)))
  ([{:keys [current-page page-size]} obj]
   (cond
     (instance? clojure.lang.Counted obj)
     (quot (dec (count obj)) page-size)

     ;; if there are no more items after the current page, we must have
     ;; reached the end of the collection, so it's not infinite.
     (empty? (drop (* (inc current-page) page-size) obj))
     current-page

     ;; possibly infinite
     :else Integer/MAX_VALUE)))

(defn- current-page
  ([inspector] (current-page inspector (:value inspector)))
  ([{:keys [current-page] :as inspector} obj]
   (let [last-page (last-page inspector obj)]
     ;; current-page might contain an incorrect value, fix that:
     (cond
       (< current-page 0) 0
       (> current-page last-page) last-page
       :else current-page))))

(defn next-page
  "Jump to the next page when inspecting a paginated sequence/map. Does nothing
  if already on the last page."
  [{:keys [current-page] :as inspector}]
  (if (>= current-page (last-page inspector))
    inspector
    (inspect-render (update inspector :current-page inc))))

(defn prev-page
  "Jump to the previous page when inspecting a paginated sequence/map. Does
  nothing if already on the first page."
  [{:keys [current-page] :as inspector}]
  (if (zero? current-page)
    inspector
    (inspect-render (update inspector :current-page dec))))

(defn up
  "Pop the stack and re-render an earlier value."
  [inspector]
  (let [{:keys [stack pages-stack]} inspector]
    (if (empty? stack)
      (inspect-render inspector)
      (-> inspector
          (update :path pop-item-from-path)
          (assoc :current-page (peek pages-stack))
          (update :pages-stack pop)
          (inspect-render (last stack))
          (update :stack pop)))))

(defn down
  "Drill down to an indexed object referred to by the previously
   rendered value."
  [inspector ^Integer idx]
  {:pre [(integer? idx)]}
  (let [idx (min idx (total-items inspector))
        page-items (items-on-page inspector)]
    (cond
      (neg? idx)
      inspector
      (> idx page-items)
      (recur (next-page inspector) (- idx page-items))
      :else
      (let [{:keys [index path current-page page-size]} inspector
            new (get index idx)
            val (:value inspector)
            new-path (push-item-to-path index idx path current-page page-size)]
        (-> inspector
            (update :stack conj val)
            (update :pages-stack conj current-page)
            (assoc :path new-path)
            (inspect-render new))))))

(defn- sibling* [inspector offset pred]
  (let [path (:path inspector)
        last-item (peek path)]
    (or (when (and (seq? last-item)
                   (= 'nth (first last-item)))
          (let [new-index (+ offset ;; for our purposes, +2 means inc, +1 nop, and +0 dec
                             (second last-item))
                top (up inspector)]
            (when (pred new-index top)
              (some-> top
                      (assoc :current-page 0)
                      (down new-index)
                      inspect-render))))
        ;; if no changes were possible, return the inspector as-is so that the UI remains untouched:
        inspector)))

(defn previous-sibling
  "Decrement the index of the last 'nth in the path by 1,
  if applicable, and re-render the updated value."
  [inspector]
  (sibling* inspector 0 (fn [idx _inspector]
                          (pos? idx))))

(defn next-sibling
  "Increment the index of the last 'nth in the path by 1,
  if applicable, and re-render the updated value."
  [inspector]
  (sibling* inspector 2 (fn [idx inspector]
                          (<= idx (total-items inspector)))))

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

(defn set-max-value-length
  "Set the maximum length of a whole printed value before it is truncated."
  [inspector max-value-length]
  {:pre [(integer? max-value-length)]}
  (inspect-render (assoc inspector :max-value-length max-value-length)))

(defn set-max-coll-size
  "Set the maximum number of nested collection members to print before truncating."
  [inspector max-coll-size]
  {:pre [(integer? max-coll-size)]}
  (inspect-render (assoc inspector :max-coll-size max-coll-size)))

(defn set-max-nested-depth
  "Set the maximum level of nested collections to print before truncating."
  [inspector max-nested-depth]
  {:pre [(integer? max-nested-depth)]}
  (inspect-render (assoc inspector :max-nested-depth max-nested-depth)))

(defn def-current-value
  "Define the currently inspected value as a var with the given name in the
  provided namespace."
  [inspector namespace var-name]
  (intern namespace (symbol var-name) (:value inspector))
  (inspect-render inspector))

(defn tap-current-value
  "Tap the currently inspected value."
  [inspector]
  (maybe-tap> (:value inspector))
  (inspect-render inspector))

(defn tap-indexed
  "Tap the value found at `idx`, without navigating to it."
  [{:keys [index] :as inspector} idx]
  (maybe-tap> (get index idx))
  (inspect-render inspector))

(defn render-onto [inspector coll]
  (update inspector :rendered into coll))

(defn render [inspector & values]
  (render-onto inspector values))

(defn render-ln [inspector & values]
  (-> inspector
      (render-onto values)
      (render '(:newline))))

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
        inspected-value (print/print-str value)
        expr (list :value inspected-value counter)]
    (-> inspector
        (update :index conj value)
        (update :counter inc)
        (update :rendered conj expr))))

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
  "Render an indexed collection of values. Renders all values in `chunk`, so
  `chunk` must be finite."
  ([inspector chunk] (render-indexed-values inspector chunk 0))
  ([inspector chunk idx-starts-from]
   (let [n (count chunk)
         last-idx (+ idx-starts-from n -1)
         last-idx-len (count (str last-idx))
         idx-fmt (str "%" last-idx-len "s")]
     (loop [ins inspector, chunk (seq chunk), idx idx-starts-from]
       (if chunk
         (recur (-> ins
                    (render-indent (format idx-fmt idx) ". ")
                    (render-value (first chunk))
                    (render-ln))
                (next chunk) (inc idx))
         ins)))))

(declare known-types)

(defn- render-page-info [{:keys [current-page page-size] :as inspector} obj]
  (if-not (#{:coll :array} (known-types inspector obj))
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
        chunk-to-display (chunk-to-display inspector obj)]
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

      (assoc ins :current-page current-page))))

(defn render-meta-information [inspector obj]
  (if (seq (meta obj))
    (-> inspector
        (render-section-header "Meta Information")
        (indent)
        (render-map-values (meta obj))
        (unindent))
    inspector))

(defn- nav-datafy-tx [obj remove-nil-valued-entries?]
  (keep (fn [[k v]]
          (or (some->> (nav obj k v)
                       (datafy)
                       (vector k))
              (when (and (nil? v)
                         (not remove-nil-valued-entries?))
                [k v])))))

(defn- nav-datafy [obj remove-nil-valued-entries?]
  (let [data (datafy obj)]
    (cond (map? data)
          (into {} (nav-datafy-tx obj remove-nil-valued-entries?) data)
          (or (sequential? data) (set? data))
          (map datafy data))))

(defn- render-datafy? [inspector obj]
  (cond (not misc/datafy?)
        false
        (map? obj)
        (not= obj (nav-datafy obj false))
        (or (sequential? obj) (set? obj))
        (not= (chunk-to-display inspector obj)
              (map datafy (chunk-to-display inspector obj)))
        :else (not= obj (datafy obj))))

(declare inspect)

(defn- render-datafy-content [inspector obj]
  (let [contents (nav-datafy obj true)]
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
          inspector (string/split-lines s)))

(defmethod inspect :string [inspector ^java.lang.String obj]
  (-> (render-class-name inspector obj)
      (render "Value: " (print/print-str obj))
      (render-ln)
      (render-section-header "Print")
      (indent)
      (render-indent-str-lines obj)
      (unindent)))

(defn- field-val [^Field f, obj]
  (try
    (.get f obj)
    (catch Exception _
      (try
        (.setAccessible f true)
        (.get f obj)
        (catch Exception _
          ::access-denied)))))

(defmethod inspect :default [inspector obj]
  (let [class-chain (loop [c (class obj), res ()]
                      (if c
                        (recur (.getSuperclass c) (cons c res))
                        res))
        memoized-field-val (memoize field-val)
        all-fields (mapcat #(.getDeclaredFields ^Class %) class-chain)
        {static-accessible        [true true]
         non-static-accessible    [false true]
         static-nonaccessible     [true false]
         non-static-nonaccessible [false false]}
        (group-by (fn [^Field f]
                    [(Modifier/isStatic (.getModifiers f))
                     (not= ::access-denied (memoized-field-val f obj))])
                  all-fields)]
    (letfn [(render-fields [inspector section-name fields]
              (if (seq fields)
                (-> inspector
                    (render-section-header section-name)
                    (indent)
                    (render-map-values (->> fields
                                            (map (fn [^Field f]
                                                   (let [v (memoized-field-val f obj)]
                                                     [(-> f .getName symbol)
                                                      (if (= v ::access-denied)
                                                        ;; This is a special value that can be detected client-side:
                                                        (symbol "<non-inspectable value>")
                                                        v)])))
                                            (into (sorted-map))))
                    (unindent))
                inspector))]
      (cond-> inspector
        true                           (render-labeled-value "Class" (class obj))
        true                           (render-labeled-value "Value" obj)
        (seq non-static-accessible)    (render-fields "Instance fields" non-static-accessible)
        (seq static-accessible)        (render-fields "Static fields" static-accessible)
        (seq non-static-nonaccessible) (render-fields "Private instance fields" non-static-nonaccessible)
        (seq static-nonaccessible)     (render-fields "Private static fields" static-nonaccessible)
        true                           (render-datafy obj)))))

(defn- render-class-section [inspector [section elements sort-key-fn]]
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
                      (sort-by sort-key-fn elements)))))

(defmethod inspect :class [inspector ^Class obj]
  (-> (reduce render-class-section
              (-> inspector
                  (render-labeled-value "Name" (-> obj .getName symbol))
                  (render-class-name obj))
              [[:Interfaces,   (.getInterfaces obj),   #(.getName ^Class %)]
               [:Constructors, (.getConstructors obj), #(.toGenericString ^Constructor %)]
               [:Fields,       (.getFields obj),       #(.getName ^Field %)]
               [:Methods,      (.getMethods obj),      #(vector (.getName ^Method %)
                                                                (.toGenericString ^Method %))]])
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
          (render-indent (string/join " " (:path inspector)))
          (unindent))
      inspector)))

(defn inspect-render
  ([{:keys [max-atom-length max-value-length max-coll-size max-nested-depth value]
     :as inspector}]
   (binding [print/*max-atom-length*  max-atom-length
             print/*max-total-length* max-value-length
             *print-length*           max-coll-size
             *print-level*            max-nested-depth]
     (-> inspector
         (reset-render-state)
         (render-reference)
         (inspect value)
         (render-page-info value)
         (render-path)
         (update :rendered seq))))
  ([inspector value]
   (inspect-render (assoc inspector :value value))))

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
     (doseq [component (:rendered (start x))]
       (inspect-print-component component)))))
