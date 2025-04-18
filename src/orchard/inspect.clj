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
   [clojure.string :as str]
   [orchard.inspect.analytics :as analytics]
   [orchard.print :as print])
  (:import
   (java.lang.reflect Constructor Field Method Modifier)
   (java.util Arrays List Map)))

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

(def ^:private supported-view-modes #{:normal :object :table})

(def ^:private default-inspector-config
  "Default configuration values for the inspector."
  {:page-size        32      ; = Clojure's default chunked sequences chunk size.
   :max-atom-length  150
   :max-value-length 10000   ; To avoid printing huge graphs and Exceptions.
   :max-coll-size    5
   :max-nested-depth nil
   :display-analytics-hint nil
   :analytics-size-cutoff  100000
   :pretty-print false})

(defn- reset-render-state [inspector]
  (-> inspector
      (assoc :counter 0, :index [], :indentation 0, :rendered [])
      (dissoc :chunk :start-idx :last-page)))

(defn- print-string
  "Print or pretty print the string `value`, depending on the view mode
  of the inspector."
  [{:keys [indentation pretty-print]} value]
  (if pretty-print
    (print/pprint-str value {:indentation (or indentation 0)})
    (print/print-str value)))

(defn- array? [obj]
  (some-> (class obj) .isArray))

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
    (instance? Throwable obj) :throwable
    (instance? StackTraceElement obj) :stack-trace-element
    (.isArray (class obj)) :array
    :else (or (:inspector-tag (meta obj))
              (type obj))))

(defmacro ^:private pre-ex [x]
  `(when-not ~x
     (throw (ex-info (str "Precondition failed: " (pr-str '~x)) {}))))

(defn- counted-length [obj]
  (cond (instance? clojure.lang.Counted obj) (count obj)
        (instance? Map obj) (.size ^Map obj)
        (array? obj) (java.lang.reflect.Array/getLength obj)
        ;; Count small lazy collections <= 10 elements (arbitrary).
        (sequential? obj) (let [bc (bounded-count 11 obj)]
                            (when (<= bc 10)
                              bc))))

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
        (dissoc :value-analysis)
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
        (dissoc :value-analysis)
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
                                max-coll-size max-nested-depth display-analytics-hint
                                analytics-size-cutoff pretty-print]
                         :as config}]
  (when (some? page-size) (pre-ex (pos-int? page-size)))
  (when (some? max-atom-length) (pre-ex (pos-int? max-atom-length)))
  (when (some? max-value-length) (pre-ex (pos-int? max-value-length)))
  (when (some? max-coll-size) (pre-ex (pos-int? max-coll-size)))
  (when (some? max-nested-depth) (pre-ex (pos-int? max-nested-depth)))
  (when (some? display-analytics-hint) (pre-ex (= display-analytics-hint "true")))
  (when (some? analytics-size-cutoff) (pre-ex (pos-int? analytics-size-cutoff)))
  (when (some? pretty-print) (pre-ex (contains? #{true false} pretty-print)))
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

(defn display-analytics
  "Calculates and renders analytics for the current object."
  [{:keys [analytics-size-cutoff value] :as inspector}]
  (inspect-render
   (if (analytics/can-analyze? value)
     (-> inspector
         (assoc :value-analysis
                (binding [analytics/*size-cutoff* analytics-size-cutoff]
                  (analytics/analytics value)))
         (dissoc :display-analytics-hint))
     inspector)))

(defn render-onto [inspector coll]
  (letfn [(render-one [{:keys [rendered] :as inspector} val]
            ;; Special case: fuse two last strings together.
            (let [lst (peek (or rendered []))]
              (assoc inspector :rendered (if (and (string? lst) (string? val))
                                           (conj (pop rendered) (str lst val))
                                           (conj rendered val)))))]
    (reduce render-one inspector coll)))

(defn render [inspector & values]
  (render-onto inspector values))

(defn render-ln [inspector & values]
  (-> inspector
      (render-onto values)
      (render '(:newline))))

(defn- indent
  "Increment the `:indentation` of `inspector` by `n` or 2."
  [inspector & [n]]
  (update inspector :indentation + (or n 2)))

(defn- unindent
  "Decrement the `:indentation` of `inspector` by `n` or 2."
  [inspector & [n]]
  (indent inspector (- (or n 2))))

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
         display-value (or display-value (print-string inspector value))
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

(defn render-labeled-value [{:keys [pretty-print] :as inspector} label value & [value-opts]]
  (let [formatted-label (str label ": ")
        indentation (if pretty-print (count formatted-label) 0)]
    (-> inspector
        (render-indent formatted-label)
        (indent indentation)
        (render-value value value-opts)
        (unindent indentation)
        (render-ln))))

(defn- render-class-name [inspector obj]
  (render-labeled-value inspector "Class" (class obj)))

(defn- render-counted-length [inspector obj]
  (if-let [clength (counted-length obj)]
    (-> inspector
        (render-indent "Count: " (str clength))
        (render-ln))
    inspector))

(defn- long-map-key?
  "Returns true of `s` is a long string, more than 20 character or
  containing newlines."
  [^String s]
  (or (.contains s "\n") (> (count s) 20)))

(defn- render-map-separator
  "Render the map separator according to `rendered-key`. If
  `rendered-key` is long or contains newlines the key and value will
  be rendered on separate lines."
  [{:keys [pretty-print] :as inspector} long-key?]
  (if (and pretty-print long-key?)
    (-> (render-ln inspector)
        (render-indent "=")
        (render-ln))
    (render inspector " = ")))

(defn- render-map-value
  "Render a map value. If `mark-values?` is true, attach the keys to the
  values in the index."
  [{:keys [pretty-print] :as inspector} key val mark-values? rendered-key long-key?]
  (if pretty-print
    (let [indentation (if long-key? 0 (+ 3 (count rendered-key)))]
      (-> (indent inspector indentation)
          (render (if (zero? indentation) "  " ""))
          (render-value val
                        (when mark-values?
                          {:value-role :map-value, :value-key key}))
          (unindent indentation)
          ((if (long-map-key? rendered-key) render-ln identity))))
    (render-value inspector val
                  (when mark-values?
                    {:value-role :map-value, :value-key key}))))

(defn- render-map-values
  "Render associative key-value pairs. If `mark-values?` is true, attach the keys
  to the values in the index."
  [inspector mappable mark-values?]
  (reduce (fn [ins [key val]]
            (let [rendered-key (print-string ins key)
                  long-key? (long-map-key? rendered-key)]
              (-> (render-indent ins)
                  (render-value key {:display-value rendered-key})
                  (render-map-separator long-key?)
                  (render-map-value key val mark-values? rendered-key long-key?)
                  (render-ln))))
          inspector
          mappable))

(defn supports-table-view-mode?
  "Return whether the inspected object can be rendered in :table view-mode."
  [{:keys [chunk value] :as _inspector}]
  (let [val (or chunk value)]
    (and (#{:list :array} (object-type val))
         (#{:list :array :map} (object-type (first val))))))

(defn- render-chunk-as-table [inspector chunk idx-starts-from]
  (let [m-i map-indexed
        fst (first chunk)
        ;; If items are maps, use map keys as keys. Otherwise assume items are
        ;; lists/vectors, so we use indices as keys.
        getter (if (map? fst) get nth)
        ks (vec (if (map? fst)
                  (keys fst)
                  (range (count fst))))
        pr-rows (into []
                      (m-i (fn [i row]
                             (let [i (+ i idx-starts-from)]
                               (into [[i (str i)]]
                                     (map (fn [k]
                                            (let [v (getter row k)]
                                              [v (print/print-str v)])))
                                     ks))))
                      chunk)
        pr-ks (into [["#" "#"]] (map (fn [k] [k (print/print-str k)])) ks)

        widths (m-i (fn [i [_ pr-k]]
                      (apply max (count pr-k)
                             (map #(count (second (nth % i))) pr-rows)))
                    pr-ks)
        pad #(String. (doto (char-array %1) (Arrays/fill ^char %2)))
        spacers (mapv #(fn [c] (pad (- % c) \space)) widths)
        divider (format "|-%s-|" (str/join "-+-" (map #(pad % \-) widths)))

        render-row #(as-> %1 ins
                      (render-indent ins "| ")
                      (reduce-kv (fn [ins i [val pr-val]]
                                   (-> ins
                                       (render ((nth spacers i) (count pr-val)))
                                       (render-value val {:display-value pr-val})
                                       (render " | ")))
                                 ins %2)
                      (render-ln ins))]
    (as-> inspector ins
      (render-ln ins)
      (render-row ins pr-ks)
      (render-indent ins)
      (render-ln ins divider)
      (reduce render-row ins pr-rows))))

(defn- render-indexed-chunk
  "Render an indexed chunk of values. Renders all values in `chunk`, so `chunk`
  must be finite. If `mark-values?` is true, attach the indices to the values in
  the index."
  [{:keys [pretty-print] :as inspector} chunk idx-starts-from mark-values?]
  (let [n (count chunk)
        last-idx (+ idx-starts-from n -1)
        last-idx-len (count (str last-idx))
        idx-fmt (str "%" last-idx-len "s")]
    (loop [ins inspector, chunk (seq chunk), idx idx-starts-from]
      (if chunk
        (let [header (str (format idx-fmt idx) ". ")
              indentation (if pretty-print (count header) 0)]
          (recur (-> ins
                     (render-indent header)
                     (indent indentation)
                     (render-value (first chunk)
                                   (when mark-values?
                                     {:value-role :seq-item, :value-key idx}))
                     (unindent indentation)
                     (render-ln))
                 (next chunk) (inc idx)))
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
    (if (and (= (:view-mode inspector) :table) (supports-table-view-mode? inspector))
      (render-chunk-as-table inspector items start-idx)
      (render-indexed-chunk inspector items start-idx mark-values?))))

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

(defn- render-analytics
  [{:keys [display-analytics-hint value-analysis] :as inspector}]
  (if (or value-analysis display-analytics-hint)
    (as-> inspector ins
      (render-section-header ins "Analytics")
      (indent ins)
      (if value-analysis
        (render-value-maybe-expand ins value-analysis)
        (-> ins
            (render-indent)
            (render-ln "Press 'y' or M-x cider-inspector-display-analytics to analyze this value.")))
      (unindent ins))
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
      (render-analytics)
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
          inspector (str/split-lines s)))

(defmethod inspect :string [inspector ^java.lang.String obj]
  (-> (render-class-name inspector obj)
      (render "Value: " (print-string inspector obj))
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
  (as-> member-string s
    (str/replace s #"[\w\.]+\.(\w+\()" "$1") ;; remove class from method name
    (str/replace s #"[\w\.]+\.(\w+)$" "$1") ;; remove class from field name
    ;; Class might not have a canonical name, as per `.getCanonicalName` doc.
    (if-let [canonical (.getCanonicalName class)]
      (str/replace s canonical (.getSimpleName class))
      s)
    (str/replace s #"java.lang.([A-Z])" "$1")))

(defmethod inspect :default [inspector obj]
  (let [class-chain (loop [c (class obj), res ()]
                      (if c
                        (recur (.getSuperclass c) (cons c res))
                        res))
        all-fields (mapcat #(.getDeclaredFields ^Class %) class-chain)
        field-values (mapv (fn [^Field f]
                             {:name (symbol (.getName f)), :value (field-val f obj)
                              :static (Modifier/isStatic (.getModifiers f))})
                           all-fields)
        {static-accessible        [true true]
         non-static-accessible    [false true]
         static-nonaccessible     [true false]
         non-static-nonaccessible [false false]}
        (group-by (fn [{:keys [static value]}]
                    ;; Be careful to use identical? instead of = because an
                    ;; object might not implement equiv().
                    [static (not (identical? ::access-denied value))])
                  field-values)
        ;; This is fine like this for now. If this condp ever grows bigger,
        ;; consider refactoring it into something polymorphic.
        printed (cond
                  (instance? Constructor obj)
                  (shorten-member-string (str obj) (.getDeclaringClass ^Constructor obj))

                  (instance? Method obj)
                  (shorten-member-string (str obj) (.getDeclaringClass ^Method obj))

                  (instance? Field obj)
                  (shorten-member-string (str obj) (.getDeclaringClass ^Field obj)))]
    (letfn [(render-fields [inspector section-name field-values]
              (if (seq field-values)
                (-> inspector
                    (render-section-header section-name)
                    (indent)
                    (render-map-values
                     (->> field-values
                          (map (fn [{:keys [name value]}]
                                 [name
                                  (if (identical? value ::access-denied)
                                    ;; This is a special value that can be
                                    ;; detected client-side:
                                    (symbol "<non-inspectable value>")
                                    value)]))
                          (into (sorted-map)))
                     false)
                    (unindent))
                inspector))
            (render-ident-hashcode [inspector]
              (let [code (System/identityHashCode obj)]
                (-> inspector
                    (render-indent "Identity hash code: " (str code) " "
                                   (format "(0x%s)" (Integer/toHexString code)))
                    (render-ln))))]
      (cond-> inspector
        true                           (render-labeled-value "Class" (class obj))
        true                           (render-labeled-value "Value" obj {:display-value printed})
        true                           (render-ident-hashcode)
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

(defn- render-stacktrace-cause [inspector ^Throwable cause]
  (as-> inspector ins
    (render-indent ins (or (.getMessage cause) "(No message)"))
    (render-ln ins)
    (render-indent ins)
    (render-value ins (class cause))
    (if-let [first-frame (first (.getStackTrace cause))]
      (-> ins
          (render " at ")
          (render-value first-frame))
      ins)
    (render-ln ins)
    (if-let [data (not-empty (ex-data cause))]
      (-> ins
          (indent)
          (render-value-maybe-expand data)
          (unindent))
      ins)))

(defmethod inspect :throwable [inspector ^Throwable obj]
  (let [causes (vec (take-while some? (iterate #(.getCause ^Throwable %) obj)))
        root-cause ^Throwable (peek causes)]
    (as-> inspector ins
      (render-labeled-value ins "Class" (class obj))
      (render-indent ins "Message: " (.getMessage obj))
      (render-ln ins)
      (render-section-header ins "Causes")
      (indent ins)
      (render-stacktrace-cause ins (first causes))
      (reduce #(render-stacktrace-cause (render-ln %1) %2) ins (rest causes))
      (unindent ins)
      (render-section-header ins "Trace")
      (indent ins)
      (render-items ins (.getStackTrace root-cause) false 0 false)
      (unindent ins)
      (render-datafy ins))))

(defmethod inspect :stack-trace-element [inspector ^StackTraceElement obj]
  (-> inspector
      (render-labeled-value "Class" (class obj))
      (render-section-header "Contents")
      (indent)
      (render-items (StackTraceElement->vec obj) false 0 false)))

(defmethod inspect :aref [inspector ^clojure.lang.ARef obj]
  (let [val (deref obj)]
    (-> (render-class-name inspector obj)
        (render-meta-information obj)
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
          (render-indent (str/join " " (:path inspector)))
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
  ([{:keys [max-atom-length max-value-length max-coll-size max-nested-depth value pretty-print]
     :as inspector}]
   (binding [print/*max-atom-length*  max-atom-length
             print/*max-total-length* max-value-length
             *print-length*           max-coll-size
             *print-level*            (cond-> max-nested-depth
                                        ;; In pretty mode a higher *print-level*
                                        ;; leads to better results, otherwise we
                                        ;; render a ton of # characters when
                                        ;; there is still enough screen estate
                                        ;; in most cases.
                                        (and pretty-print (number? max-nested-depth))
                                        (* 2))]
     (-> inspector
         (reset-render-state)
         (decide-if-paginated)
         (inspect value)
         (render-path)
         (render-view-mode)
         (update :rendered seq))))
  ([inspector value]
   (inspect-render (-> (assoc inspector :value value)
                       (dissoc :value-analysis)))))

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
