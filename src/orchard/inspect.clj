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
   [clojure.core.protocols :refer [datafy nav]]
   [clojure.string :as string]
   [orchard.print :as print])
  (:import
   (java.lang.reflect Constructor Field Method Modifier)
   (java.util List Map)))

;;
;; Navigating Inspector State
;;

(declare inspect-render)

(defn push-item-to-path
  "Takes `path` and the role and key of the value to be navigated to, and returns
  the updated path to the selected value."
  [path role key]
  (case role
    :seq-item (conj path (list 'nth key))
    :map-value (conj path (if (keyword? key)
                            key
                            (list 'get key)))
    (conj path '<unknown>)))

(def ^:private supported-view-modes #{:normal :object})

(def ^:private default-inspector-config
  "Default configuration values for the inspector."
  {:page-size        32      ; = Clojure's default chunked sequences chunk size.
   :max-atom-length  150
   :max-value-length 10000   ; To avoid printing huge graphs and Exceptions.
   :max-coll-size    5
   :max-nested-depth nil})

(defn- reset-render-state [inspector]
  (-> inspector
      (assoc :counter 0, :index [], :indentation 0, :rendered [])
      (dissoc :chunk :start-idx :last-page)))

(defn- array? [obj]
  (.isArray (class obj)))

(defn- object-type [obj]
  (cond
    (nil? obj) :nil
    (string? obj) :string
    (set? obj) :set
    (instance? Map obj) :map
    (instance? List obj) :list
    (var? obj) :var
    (instance? Class obj) :class
    (instance? clojure.lang.Namespace obj) :namespace
    (instance? clojure.lang.ARef obj) :aref
    (.isArray (class obj)) :array
    :else (or (:inspector-tag (meta obj))
              (type obj))))

(defmacro ^:private pre-ex [x]
  `(when-not ~x
     (throw (ex-info (str "Precondition failed: " (pr-str '~x)) {}))))

(defn- counted-length [obj]
  (cond (instance? clojure.lang.Counted obj) (count obj)
        (array? obj) (java.lang.reflect.Array/getLength obj)))

(defn- pagination-info
  "Calculate if the object should be paginated given the page size. Return a map
  with pagination info, or nil if object fits in a single page."
  [obj page-size current-page]
  (let [clength (counted-length obj)
        start-idx (* current-page page-size)
        ;; Try grab a chunk that is one element longer than asked in
        ;; page-size. This is how we know there are elements beyond the
        ;; current page.
        chunk+1 (->> obj
                     (drop start-idx)
                     (take (inc page-size)))
        count+1 (count chunk+1)
        paginate? (or (> current-page 0) ;; In non-paginated it's always 0.
                      (> count+1 page-size))
        last-page (cond clength (quot (dec clength) page-size)
                        (<= count+1 page-size) current-page
                        ;; Possibly infinite
                        :else Integer/MAX_VALUE)]
    (when paginate?
      {:chunk (take page-size chunk+1)
       :start-idx start-idx
       :last-page last-page})))

(defn- decide-if-paginated
  "Make early decision if the inspected object should be paginated. If so,
  assoc the `:chunk` to be displayed to `inspector`."
  [{:keys [value current-page page-size] :as inspector}]
  (let [pageable? (boolean (#{:list :map :set :array} (object-type value)))]
    (cond-> (assoc inspector :pageable pageable?)
      pageable? (merge (pagination-info value page-size current-page)))))

(defn next-page
  "Jump to the next page when inspecting a paginated sequence/map. Does nothing
  if already on the last page."
  [{:keys [current-page last-page] :as inspector}]
  (if (some-> last-page (> current-page))
    (inspect-render (update inspector :current-page inc))
    inspector))

(defn prev-page
  "Jump to the previous page when inspecting a paginated sequence/map. Does
  nothing if already on the first page."
  [{:keys [current-page] :as inspector}]
  (if (zero? current-page)
    inspector
    (inspect-render (update inspector :current-page dec))))

(defn- up*
  "Move one value up the stack without re-rendering."
  [{:keys [stack pages-stack view-modes-stack] :as inspector}]
  (if (empty? stack)
    inspector
    (-> inspector
        (update :path pop)
        (assoc :current-page (peek pages-stack))
        (update :pages-stack pop)
        (assoc :view-mode (peek view-modes-stack))
        (update :view-modes-stack pop)
        (assoc :value (peek stack))
        (update :stack pop))))

(defn up
  "Pop the stack and re-render an earlier value."
  [inspector]
  (inspect-render (up* inspector)))

(defn- down*
  "Navigate to `child` value of the current inspector without re-rendering."
  [inspector child child-role child-key]
  (let [{:keys [value current-page page-size view-mode]} inspector
        ;; If down* was called on an invisible element (e.g. by sibling*),
        ;; :current-page may be wrong, recompute it.
        current-page (if (number? child-key)
                       (quot child-key page-size)
                       current-page)]
    (-> inspector
        (assoc :value child)
        (update :stack conj value)
        (assoc :current-page 0)
        (update :pages-stack conj current-page)
        (assoc :view-mode :normal)
        (update :view-modes-stack conj view-mode)
        (update :path push-item-to-path child-role child-key))))

(defn down
  "Drill down to an indexed object referred to by the previously rendered value."
  [inspector idx]
  (pre-ex (int? idx))
  (if-let [{:keys [value role key]} (get (:index inspector) idx)]
    (inspect-render (down* inspector value role key))
    inspector))

(defn- sibling* [inspector offset]
  (let [path (:path inspector)
        last-item (peek path)]
    (or (when (and (seq? last-item)
                   (= 'nth (first last-item)))
          (let [new-index (+ (second last-item) offset)
                top (up* inspector)
                sibling (try (nth (:value top) new-index ::not-found)
                             ;; Exception may happen if the collection
                             ;; rendered as sequential doesn't support nth.
                             (catch Exception _ ::not-found))]
            (when-not (= sibling ::not-found)
              (-> top
                  (down* sibling :seq-item new-index)
                  inspect-render))))
        ;; if no changes were possible, return the inspector as-is so that the UI remains untouched:
        inspector)))

(defn previous-sibling
  "Attempt to inspect the item previous to the currenlty inspected value in the
  parent sequential collection."
  [inspector]
  (sibling* inspector -1))

(defn next-sibling
  "Attempt to inspect the item next to the currenlty inspected value in the parent
  sequential collection."
  [inspector]
  (sibling* inspector 1))

(defn- validate-config [{:keys [page-size max-atom-length max-value-length
                                max-coll-size max-nested-depth]
                         :as config}]
  (when (some? page-size) (pre-ex (pos-int? page-size)))
  (when (some? max-atom-length) (pre-ex (pos-int? max-atom-length)))
  (when (some? max-value-length) (pre-ex (pos-int? max-value-length)))
  (when (some? max-coll-size) (pre-ex (pos-int? max-coll-size)))
  (when (some? max-nested-depth) (pre-ex (pos-int? max-nested-depth)))
  (select-keys config (keys default-inspector-config)))

(defn refresh
  "Update inspector configuration with values in `config-override` and re-render.
  Unspecified config values remain the same. Supported config keys:

  `:page-size` - page size in pagination mode
  `:max-atom-length` - maximum length of atomic value before truncating
  `:max-value-length` - maximum length of a whole printed value before truncating
  `:max-coll-size` - maximum number of collection items to print before truncating
  `:max-nested-depth` - maximum nesting level to print before truncating"
  [inspector config-override]
  (as-> (validate-config config-override) config
    ;; If page size is changed, reset the current page.
    (if (contains? config :page-size)
      (assoc config :current-page 0)
      config)
    (inspect-render (merge inspector config))))

(defn def-current-value
  "Define the currently inspected value as a var with the given name in the
  provided namespace."
  [inspector namespace var-name]
  (intern namespace (symbol var-name) (:value inspector))
  (inspect-render inspector))

(defn tap-current-value
  "Tap the currently inspected value."
  [inspector]
  (tap> (:value inspector))
  (inspect-render inspector))

(defn tap-indexed
  "Tap the value found at `idx`, without navigating to it."
  [{:keys [index] :as inspector} idx]
  (tap> (:value (get index idx)))
  (inspect-render inspector))

(defn set-view-mode
  "Set the view mode for the current value to `mode`. See allowed values in
  `supported-view-modes`."
  [inspector mode]
  (pre-ex (contains? supported-view-modes mode))
  (inspect-render (assoc inspector :view-mode mode)))

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

(defn render-value
  "Render the given `value` and add it to the index. `value-role` can be provided
  to mark in the index that the value comes from a collection, and `value-key`
  stands for the key by which value can be retrieved from its collection.
  `display-value` string can be provided explicitly."
  ([inspector value] (render-value inspector value nil))
  ([inspector value {:keys [value-role value-key display-value]}]
   (let [{:keys [counter]} inspector
         display-value (or display-value (print/print-str value))
         expr (list :value display-value counter)]
     (-> inspector
         (update :index conj {:value value
                              :role value-role
                              :key value-key})
         (update :counter inc)
         (update :rendered conj expr)))))

(defn render-indented-value [inspector value & [value-opts]]
  (-> inspector
      (render-indent)
      (render-value value value-opts)
      (render-ln)))

(defn render-labeled-value [inspector label value & [value-opts]]
  (-> inspector
      (render-indent (str label ": "))
      (render-value value value-opts)
      (render-ln)))

(defn- render-class-name [inspector obj]
  (render-labeled-value inspector "Class" (class obj)))

(defn- render-counted-length [inspector obj]
  (if-let [clength (counted-length obj)]
    (-> inspector
        (render-indent "Count: " (str clength))
        (render-ln))
    inspector))

(defn- render-map-values
  "Render associative key-value pairs. If `mark-values?` is true, attach the keys
  to the values in the index."
  [inspector mappable mark-values?]
  (reduce (fn [ins [key val]]
            (-> ins
                (render-indent)
                (render-value key)
                (render " = ")
                (render-value val (when mark-values?
                                    {:value-role :map-value, :value-key key}))
                (render-ln)))
          inspector
          mappable))

(defn- render-indexed-chunk
  "Render an indexed chunk of values. Renders all values in `chunk`, so `chunk`
  must be finite. If `mark-values?` is true, attach the indices to the values in
  the index."
  [inspector chunk idx-starts-from mark-values?]
  (let [n (count chunk)
        last-idx (+ idx-starts-from n -1)
        last-idx-len (count (str last-idx))
        idx-fmt (str "%" last-idx-len "s")]
    (loop [ins inspector, chunk (seq chunk), idx idx-starts-from]
      (if chunk
        (recur (-> ins
                   (render-indent (format idx-fmt idx) ". ")
                   (render-value (first chunk)
                                 (when mark-values?
                                   {:value-role :seq-item, :value-key idx}))
                   (render-ln))
               (next chunk) (inc idx))
        ins))))

(declare known-types)

(defn- render-page-info
  [{:keys [current-page last-page page-size] :as inspector}]
  (if last-page
    (-> (render-section-header inspector "Page Info")
        (indent)
        (render-indent (format "Page size: %d, showing page: %d of %s"
                               page-size (inc current-page)
                               (if (= last-page Integer/MAX_VALUE)
                                 "?" (inc last-page))))
        (unindent)
        (render-ln))
    inspector))

(defn- render-items [inspector items map? start-idx mark-values?]
  (if map?
    (render-map-values inspector items mark-values?)
    (render-indexed-chunk inspector items start-idx mark-values?)))

(defn- render-value-maybe-expand
  "If `obj` is a collection smaller than page-size, then render it as a
  collection, otherwise as a compact value."
  [{:keys [page-size] :as inspector} obj]
  (if (some-> (counted-length obj) (<= page-size))
    (render-items inspector obj (map? obj) 0 false)
    (render-indented-value inspector obj)))

(defn- render-leading-page-ellipsis [{:keys [current-page] :as inspector}]
  (if (> current-page 0)
    (-> inspector
        (render-indent "...")
        (render-ln))
    inspector))

(defn- render-trailing-page-ellipsis
  [{:keys [current-page last-page] :as inspector}]
  (if (some-> last-page (> current-page))
    (-> inspector
        (render-indent "...")
        (render-ln))
    inspector))

(defn- render-collection-paged
  "Render a single page of either an indexed or associative collection."
  [{:keys [value chunk start-idx] :as inspector}]
  (let [type (object-type value)]
    (-> inspector
        (render-leading-page-ellipsis)
        (render-items (or chunk value) (= type :map) (or start-idx 0)
                      ;; Set items are not indexed - don't mark.
                      (not= type :set))
        (render-trailing-page-ellipsis))))

(defn render-meta-information [inspector obj]
  (if (seq (meta obj))
    (-> inspector
        (render-section-header "Meta Information")
        (indent)
        (render-value-maybe-expand (meta obj))
        (unindent))
    inspector))

;;;; Datafy

(defn- datafy-kvs [original-object kvs]
  (let [differs? (volatile! false)
        result (into {}
                     (keep (fn [[k v]]
                             (when-some [dat (some->> (nav original-object k v)
                                                      datafy)]
                               (when-not (= dat v)
                                 (vreset! differs? true))
                               [k dat])))
                     kvs)]
    (with-meta result {:differs @differs?})))

(defn- datafy-seq [s]
  (let [differs? (volatile! false)
        result (mapv #(let [dat (datafy %)]
                        (when-not (= dat %)
                          (vreset! differs? true))
                        dat) s)]
    (with-meta result {:differs @differs?})))

(defn- datafy-root [obj]
  (let [datafied (datafy obj)]
    (when-not (identical? obj datafied)
      datafied)))

(defn- datafy-displayed-value
  "Datafy either the current value or its paginated view. Return datafied
  representation if it differs from value and boolean `mirror?` that tells if
  the datafied representation mirrors the structure of the input collection."
  [{:keys [value chunk pageable]}]
  (if-let [datafied (datafy-root value)]
    ;; If the root value has datafy representation, check if it's a collection.
    ;; If so, additionally datafy its items or map values.
    (let [datafied (case (object-type datafied)
                     :map (datafy-kvs datafied datafied)
                     (:list :set) (datafy-seq datafied)
                     datafied)]
      ;; Only render the datafy section if the datafied version of the object is
      ;; different than object, since we don't want to show the same data twice.
      (when-not (identical? datafied value)
        [datafied false]))

    (when pageable
      ;; If the value is a type that can be paged, then only datafy the
      ;; displayed chunk.
      (let [chunk (or chunk value)
            map? (= (object-type value) :map)
            datafied (if map?
                       (datafy-kvs value chunk)
                       (datafy-seq chunk))]
        ;; Only return the datafied representation if at least one value is
        ;; different from the original.
        (when (:differs (meta datafied))
          [datafied true])))))

(defn- render-datafy [{:keys [start-idx] :as inspector}]
  (if-let [[datafied mirror?] (datafy-displayed-value inspector)]
    (as-> inspector ins
      (render-section-header ins "Datafy")
      (indent ins)

      (if mirror?
        ;; If datafy is a "mirror" of the inspected object, then display it
        ;; using the same pagination rules as the main chunk.
        (-> ins
            (render-leading-page-ellipsis)
            (render-items datafied (map? datafied) (or start-idx 0) false)
            (render-trailing-page-ellipsis))
        ;; Otherwise, render datafied representation as a collection if it is
        ;; small enough, or as a single value.
        (render-value-maybe-expand ins datafied))

      (unindent ins))
    inspector))

;; Inspector multimethod
(defn- dispatch-inspect [{:keys [view-mode] :as _ins} obj]
  (if (= view-mode :object)
    ;; When :object mode is requested, render all values as unrecognized objects.
    :default
    (object-type obj)))

(defmulti inspect #'dispatch-inspect)

(defmethod inspect :nil [inspector _obj]
  (-> inspector
      (render-ln "nil")))

(defn- inspect-coll [inspector obj]
  (-> (render-class-name inspector obj)
      (render-counted-length obj)
      (render-meta-information obj)
      (render-section-header "Contents")
      (indent)
      (render-collection-paged)
      (unindent)
      (render-datafy)
      (render-page-info)))

(defmethod inspect :list [inspector obj] (inspect-coll inspector obj))
(defmethod inspect :set  [inspector obj] (inspect-coll inspector obj))
(defmethod inspect :map  [inspector obj] (inspect-coll inspector obj))

(defmethod inspect :array [inspector obj]
  (-> (render-class-name inspector obj)
      (render-counted-length obj)
      (render-labeled-value "Component Type" (.getComponentType (class obj)))
      (render-section-header "Contents")
      (indent)
      (render-collection-paged)
      (unindent)
      (render-datafy)
      (render-page-info)))

(defn- render-var-value [inspector ^clojure.lang.Var obj]
  (if-not (.isBound obj)
    inspector
    (render-labeled-value inspector "Value" (var-get obj))))

(defmethod inspect :var [inspector obj]
  (-> (render-class-name inspector obj)
      (render-var-value obj)
      (render-meta-information obj)
      (render-datafy)))

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

(defn- shorten-member-string [member-string, ^Class class]
  ;; Ugly as hell, but easier than reimplementing all custom printing that
  ;; java.lang.reflect does.
  (-> member-string
      (string/replace #"[\w\.]+\.(\w+\()" "$1") ;; remove class from method name
      (string/replace #"[\w\.]+\.(\w+)$" "$1") ;; remove class from field name
      (string/replace (.getCanonicalName class) (.getSimpleName class))
      (string/replace #"java.lang.([A-Z])" "$1")))

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
                  all-fields)
        ;; This is fine like this for now. If this condp ever grows bigger,
        ;; consider refactoring it into something polymorphic.
        printed (cond
                  (instance? Constructor obj)
                  (shorten-member-string (str obj) (.getDeclaringClass ^Constructor obj))

                  (instance? Method obj)
                  (shorten-member-string (str obj) (.getDeclaringClass ^Method obj))

                  (instance? Field obj)
                  (shorten-member-string (str obj) (.getDeclaringClass ^Field obj))

                  :else (print/print-str obj))]
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
                                            (into (sorted-map)))
                                       false)
                    (unindent))
                inspector))]
      (cond-> inspector
        true                           (render-labeled-value "Class" (class obj))
        true                           (render-labeled-value "Value" obj {:display-value printed})
        (seq non-static-accessible)    (render-fields "Instance fields" non-static-accessible)
        (seq static-accessible)        (render-fields "Static fields" static-accessible)
        (seq non-static-nonaccessible) (render-fields "Private instance fields" non-static-nonaccessible)
        (seq static-nonaccessible)     (render-fields "Private static fields" static-nonaccessible)
        true                           (render-datafy)))))

(defn- render-class-section [inspector section elements print-fn & [sort-fn]]
  (if-not (seq elements)
    inspector
    (unindent (->> (mapv #(vector % (print-fn %)) elements)
                   (sort-by (if sort-fn
                              (comp sort-fn first)
                              second))
                   (reduce (fn [ins [elt printed]]
                             (render-indented-value ins elt {:display-value printed}))
                           (-> inspector
                               (render-section-header section)
                               (indent)))))))

(defn- render-class-hierarchy [inspector ^Class klass]
  (let [seen (volatile! #{})]
    (letfn [(render-class [ins k]
              (if (@seen k)
                ins
                (do (vswap! seen conj k)
                    (-> ins
                        (render-indented-value k)
                        (indent)
                        (render-parents k)
                        (unindent)))))
            (render-parents [ins ^Class k]
              (as-> ins ins
                (if-let [super (.getSuperclass k)]
                  (render-class ins super)
                  ins)
                (reduce render-class ins
                        (sort-by #(.getName ^Class %) (.getInterfaces k)))))]
      (if (or (.getSuperclass klass) (seq (.getInterfaces klass)))
        (as-> inspector ins
          (render-section-header ins "Class hierarchy")
          (indent ins)
          (render-parents ins klass)
          (unindent ins))
        inspector))))

(defmethod inspect :class [inspector ^Class obj]
  (let [print-fn (fn [to-string]
                   #(shorten-member-string (to-string %) obj))]
    (-> inspector
        (render-labeled-value "Name" (-> obj .getName symbol))
        (render-class-name obj)
        (render-class-hierarchy obj)
        (render-class-section :Constructors (.getConstructors obj)
                              (print-fn #(.toGenericString ^Constructor %)))
        (render-class-section :Fields (.getFields obj)
                              (print-fn #(.toGenericString ^Field %)))
        (render-class-section :Methods (.getMethods obj)
                              (print-fn #(.toGenericString ^Method %))
                              #(vector (.getName ^Method %)
                                       (.toGenericString ^Method %)))
        (render-datafy))))

(defmethod inspect :aref [inspector ^clojure.lang.ARef obj]
  (let [val (deref obj)]
    (-> (render-class-name inspector obj)
        (render-section-header "Deref")
        (indent)
        (render-class-name val)
        (render-counted-length val)
        (render-section-header "Contents")
        (indent)
        (render-value-maybe-expand val)
        (unindent)
        (unindent))))

(defn ns-refers-by-ns [^clojure.lang.Namespace ns]
  (group-by (fn [^clojure.lang.Var v] (.ns v))
            (map val (ns-refers ns))))

(defn- render-ns-refers [inspector obj]
  (let [refers (ns-refers-by-ns obj)]
    (if-not (seq refers)
      inspector
      (-> (render-section-header inspector "Refer from")
          (indent)
          (render-map-values refers false)
          (unindent)))))

(defn- render-ns-imports [inspector obj]
  (let [imports (ns-imports obj)]
    (if-not (seq imports)
      inspector
      (-> (render-section-header inspector "Imports")
          (indent)
          (render-indented-value imports)
          (unindent)))))

(defn- render-ns-interns [inspector obj]
  (let [interns (ns-interns obj)]
    (if-not (seq interns)
      inspector
      (-> (render-section-header inspector "Interns")
          (indent)
          (render-indented-value interns)
          (unindent)))))

(defmethod inspect :namespace [inspector ^clojure.lang.Namespace obj]
  (-> (render-class-name inspector obj)
      (render-counted-length (ns-map obj))
      (render-meta-information obj)
      (render-ns-refers obj)
      (render-ns-imports obj)
      (render-ns-interns obj)
      (render-datafy)))

(defn render-path [inspector]
  (let [path (:path inspector)]
    (if (and (seq path) (not-any? #(= % '<unknown>) path))
      (-> (render-section-header inspector "Path")
          (indent)
          (render-indent (string/join " " (:path inspector)))
          (unindent))
      inspector)))

(defn render-view-mode [inspector]
  (let [view-mode (:view-mode inspector)]
    (if (= view-mode :normal)
      inspector
      (-> (render-section-header inspector "View mode")
          (indent)
          (render-indent (str view-mode))
          (unindent)))))

(defn inspect-render
  ([{:keys [max-atom-length max-value-length max-coll-size max-nested-depth value]
     :as inspector}]
   (binding [print/*max-atom-length*  max-atom-length
             print/*max-total-length* max-value-length
             *print-length*           max-coll-size
             *print-level*            max-nested-depth]
     (-> inspector
         (reset-render-state)
         (decide-if-paginated)
         (inspect value)
         (render-path)
         (render-view-mode)
         (update :rendered seq))))
  ([inspector value]
   (inspect-render (assoc inspector :value value))))

;; Public entrypoints

(defn start
  "Create a new inspector for the `value`. Optinally accepts a `config` map (which
  can be an existing inspector with changed config). See `refresh` for the list
  of supported keys."
  ([value] (start {} value))
  ([config value]
   (-> default-inspector-config
       (merge (validate-config config))
       (assoc :stack [], :path [], :pages-stack [], :current-page 0,
              :view-modes-stack [], :view-mode :normal)
       (inspect-render value))))

(defn ^:deprecated clear
  "If necessary, use `(start inspector nil) instead.`"
  [inspector]
  (start inspector nil))

(defn ^:deprecated fresh
  "If necessary, use `(start nil)` instead."
  []
  (start nil))

(defn ^:deprecated set-page-size
  "Use `refresh` instead."
  [inspector new-page-size]
  (refresh inspector {:page-size new-page-size}))

(defn ^:deprecated set-max-atom-length
  "Use `refresh` instead."
  [inspector max-atom-length]
  (refresh inspector {:max-atom-length max-atom-length}))

(defn ^:deprecated set-max-value-length
  "Use `refresh` instead."
  [inspector max-value-length]
  (refresh inspector {:max-value-length max-value-length}))

(defn ^:deprecated set-max-coll-size
  "Use `refresh` instead."
  [inspector max-coll-size]
  (refresh inspector {:max-coll-size max-coll-size}))

(defn ^:deprecated set-max-nested-depth
  "Use `refresh` instead."
  [inspector max-nested-depth]
  (refresh inspector {:max-nested-depth max-nested-depth}))

(defn inspect-print
  "Get a human readable printout of rendered sequence."
  [x]
  (print
   (with-out-str
     (doseq [[type value :as component] (:rendered (start x))]
       (print (case type
                :newline \newline
                :value (str value)
                component))))))
