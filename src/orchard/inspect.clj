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
   [orchard.java.compatibility :as compat]
   [orchard.pp :as pp]
   [orchard.print :as print])
  (:import
   (java.lang.reflect Constructor Field Method Modifier)
   (java.util Arrays List Map)))

;;
;; Navigating Inspector State
;;

(declare inspect-render supported-view-modes)

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
      (assoc :index [], :indentation 0, :rendered (transient []))
      (dissoc :chunk :start-idx :last-page)))

(defn- print-string
  "Print or pretty print the string `value`, depending on the view mode
  of the inspector."
  [{:keys [indentation pretty-print]} value]
  (if pretty-print
    (pp/pprint-str value {:indentation (or indentation 0)})
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

(defn- pageable? [obj]
  (contains? #{:list :map :set :array} (object-type obj)))

(defn- counted-length [{:keys [page-size]} obj]
  (cond (instance? clojure.lang.Counted obj) (count obj)
        (instance? Map obj) (.size ^Map obj)
        (array? obj) (java.lang.reflect.Array/getLength obj)
        ;; Count small lazy collections (<= page-size).
        (pageable? obj) (let [bc (bounded-count (inc page-size) obj)]
                          (when (<= bc page-size)
                            bc))))

(defn- pagination-info
  "Calculate if the object should be paginated given the page size. Return a map
  with pagination info, or nil if object fits in a single page."
  [{:keys [page-size current-page view-mode value] :as inspector}]
  (let [page-size (if (= view-mode :hex)
                    (* page-size 16) ;; In hex view mode, each row is 16 bytes.
                    page-size)
        start-idx (* current-page page-size)
        ;; Try grab a chunk that is one element longer than asked in
        ;; page-size. This is how we know there are elements beyond the
        ;; current page.
        chunk+1 (persistent! (transduce (comp (drop start-idx)
                                              (take (inc page-size)))
                                        conj! (transient []) value))
        count+1 (count chunk+1)
        paginate? (or (> current-page 0) ;; In non-paginated it's always 0.
                      (> count+1 page-size))
        clength (or (counted-length inspector value)
                    (when (<= count+1 page-size)
                      (+ (* page-size current-page) count+1)))
        last-page (if clength
                    (quot (dec clength) page-size)
                    ;; Possibly infinite
                    Integer/MAX_VALUE)]
    (when paginate?
      {:chunk (cond-> chunk+1
                (> count+1 page-size) pop)
       :start-idx start-idx
       :last-page last-page})))

(defn- decide-if-paginated
  "Make early decision if the inspected object should be paginated. If so,
  assoc the `:chunk` to be displayed to `inspector`."
  [{:keys [value] :as inspector}]
  (cond-> inspector
    (pageable? value) (merge (pagination-info inspector))))

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
                       current-page)
        ins (-> inspector
                (assoc :value child)
                (dissoc :value-analysis)
                (update :stack conj value)
                (assoc :current-page 0)
                (update :pages-stack conj current-page)
                (update :view-modes-stack conj view-mode)
                (update :path push-item-to-path child-role child-key))]
    (assoc ins :view-mode (first (supported-view-modes ins)))))

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

;; View modes

(def ^:private view-mode-order [:hex :normal :table :object])

(defmulti view-mode-supported? (fn [_inspector view-mode] view-mode))

(defmethod view-mode-supported? :normal [_ _] true)

(defmethod view-mode-supported? :object [{:keys [value]} _]
  ;; A hack - for all "known" types `object-type` returns a keyword. If it's not
  ;; a keyword, it means we render it using object renderer, so :object
  ;; view-mode is redundant for it.
  (keyword? (object-type value)))

(defmethod view-mode-supported? :table [{:keys [chunk value]} _]
  (let [chunk (or chunk value)]
    (and (#{:list :array} (object-type value))
         (#{:list :array :map} (object-type (first chunk))))))

(defmethod view-mode-supported? :hex [{:keys [value]} _]
  (when-let [klass (class value)]
    (and (.isArray klass)
         (= (.getComponentType klass) Byte/TYPE))))

(defn set-view-mode
  "Set the view mode for the current value to `mode`."
  [inspector mode]
  (pre-ex (view-mode-supported? inspector mode))
  (inspect-render (assoc inspector :view-mode mode)))

(defn- supported-view-modes [inspector]
  (filter #(view-mode-supported? inspector %) view-mode-order))

(defn toggle-view-mode
  "Switch to the next supported view mode."
  [{:keys [view-mode] :as inspector}]
  (let [supported (supported-view-modes inspector)
        transitions (zipmap supported (rest (cycle supported)))]
    (set-view-mode inspector (transitions view-mode))))

;; Rendering

(defn render
  ([inspector value]
   (update inspector :rendered conj! value))
  ([inspector value & values]
   (reduce render (render inspector value) values)))

(defn render-onto [inspector coll]
  (reduce render inspector coll))

(defn render-ln [inspector]
  (render inspector '(:newline)))

(defn- indent
  "Increment the `:indentation` of `inspector` by `n` or 2."
  ([inspector] (update inspector :indentation + 2))
  ([inspector n]
   (cond-> inspector
     (pos? n) (update :indentation + n))))

(defn- unindent
  "Decrement the `:indentation` of `inspector` by `n` or 2."
  ([inspector] (update inspector :indentation - 2))
  ([inspector n]
   (cond-> inspector
     (pos? n) (update :indentation - n))))

(defn- padding [{:keys [indentation]}]
  (when (and (number? indentation) (pos? indentation))
    (if (= indentation 2) "  " ;; Fastpath
        (String. (char-array indentation \space)))))

(defn- render-indent
  ([inspector]
   (if-let [padding (padding inspector)]
     (render inspector padding)
     inspector))
  ([inspector & values]
   (render-onto (render-indent inspector) values)))

(defn- render-indent-ln [inspector & values]
  (-> (apply render-indent inspector values)
      (render-ln)))

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
   (let [{:keys [index]} inspector
         display-value (or display-value (print-string inspector value))
         expr (seq [:value display-value (count index)])]
     (-> inspector
         (update :index conj {:value value
                              :role value-role
                              :key value-key})
         (update :rendered conj! expr)))))

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
  (if-let [clength (counted-length inspector obj)]
    (render-indent-ln inspector "Count: " (str clength))
    inspector))

(defn- long-map-key?
  "Returns true of `s` is a long string, more than 20 character or
  containing newlines."
  [^String s]
  (or (.contains s "\n") (> (count s) 50)))

(defn- render-map-separator
  "Render the map separator according to `rendered-key`. If
  `rendered-key` is long or contains newlines the key and value will
  be rendered on separate lines."
  [{:keys [pretty-print] :as inspector} long-key?]
  (if (and pretty-print long-key?)
    (-> inspector
        (render-ln)
        (render-indent-ln "="))
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
      (render-indent-ln ins divider)
      (reduce render-row ins pr-rows))))

(defn- leftpad [idx last-idx-len]
  (let [^String idx-s (str idx)
        idx-len (count idx-s)]
    (if (= idx-len last-idx-len)
      (.concat idx-s ". ")
      (str (String. (char-array (- last-idx-len idx-len) \space)) idx-s ". "))))

(defn- render-indexed-chunk
  "Render an indexed chunk of values. Renders all values in `chunk`, so `chunk`
  must be finite. If `mark-values?` is true, attach the indices to the values in
  the index. If `skip-nils?` is true, don't render nil values."
  [{:keys [pretty-print] :as inspector} chunk {:keys [start-idx mark-values? skip-nils?]}]
  (let [start-idx (or start-idx 0)
        n (count chunk)
        idx (volatile! start-idx)
        last-idx (+ start-idx n -1)
        last-idx-len (count (str last-idx))]
    (reduce (fn [ins item]
              (let [i @idx
                    header (leftpad i last-idx-len)
                    indentation (if pretty-print (count header) 0)]
                (vswap! idx inc)
                (if-not (and (nil? item) skip-nils?)
                  (-> ins
                      (render-indent)
                      (render header)
                      (indent indentation)
                      (render-value item
                                    (when mark-values?
                                      {:value-role :seq-item, :value-key i}))
                      (unindent indentation)
                      (render-ln))
                  ins)))
            inspector chunk)))

(declare known-types)

(defn- render-page-info
  [{:keys [current-page last-page page-size] :as inspector}]
  (if last-page
    (-> (render-section-header inspector "Page Info")
        (indent)
        (render-indent-ln (format "Page size: %d, showing page: %d of %s"
                                  page-size (inc current-page)
                                  (if (= last-page Integer/MAX_VALUE)
                                    "?" (inc last-page))))
        (unindent))
    inspector))

(defn- render-items
  [inspector items {:keys [map? start-idx mark-values?] :as opts}]
  (if map?
    (render-map-values inspector items mark-values?)
    (if (= (:view-mode inspector) :table)
      (render-chunk-as-table inspector items (or start-idx 0))
      (render-indexed-chunk inspector items opts))))

(defn- render-value-maybe-expand
  "If `obj` is a collection smaller than page-size, then render it as a
  collection, otherwise as a compact value."
  [{:keys [page-size] :as inspector} obj]
  (if (some-> (counted-length inspector obj) (<= page-size))
    (render-items inspector obj {:map? (map? obj), :start-idx 0})
    (render-indented-value inspector obj)))

(defn- render-leading-page-ellipsis [{:keys [current-page] :as inspector}]
  (if (> current-page 0)
    (render-indent-ln inspector "...")
    inspector))

(defn- render-trailing-page-ellipsis
  [{:keys [current-page last-page] :as inspector}]
  (if (some-> last-page (> current-page))
    (render-indent-ln inspector "...")
    inspector))

(defn- render-collection-paged
  "Render a single page of either an indexed or associative collection."
  [{:keys [value chunk start-idx] :as inspector}]
  (let [type (object-type value)]
    (-> inspector
        (render-leading-page-ellipsis)
        (render-items (or chunk value)
                      {:map? (= type :map)
                       :start-idx start-idx
                       ;; Set items are not indexed - don't mark.
                       :mark-values? (not= type :set)})
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
        (render-indent-ln
         ins "Press 'y' or M-x cider-inspector-display-analytics to analyze this value."))
      (unindent ins))
    inspector))

;;;; Datafy

(defn- datafy-kvs [original-object kvs keep-same?]
  ;; keep-same? should be true for datafying collections that were produced by
  ;; datafying the root, and false if we datafy elements of the original coll.
  (let [differs? (volatile! false)
        result (into {}
                     (keep (fn [[k v]]
                             (when-some [dat (some->> (nav original-object k v)
                                                      datafy)]
                               (let [same? (= dat v)]
                                 (when-not same?
                                   (vreset! differs? true))
                                 (when (or (not same?) keep-same?)
                                   [k dat])))))
                     kvs)]
    (when-not (empty? result)
      result)))

(defn- datafy-seq [s keep-same?]
  (let [differs? (volatile! false)
        result (mapv #(let [dat (datafy %)
                            same? (= dat %)]
                        (when-not same?
                          (vreset! differs? true))
                        (when (or (not same?) keep-same?)
                          dat))
                     s)]
    (when (or @differs? keep-same?)
      result)))

(defn- datafy-root [obj]
  (let [datafied (datafy obj)]
    (when-not (identical? obj datafied)
      datafied)))

(defn- datafy-displayed-value
  "Datafy either the current value or its paginated view. Return datafied
  representation if it differs from value and boolean `mirror?` that tells if
  the datafied representation mirrors the structure of the input collection."
  [{:keys [value chunk]}]
  (if-let [datafied (datafy-root value)]
    ;; If the root value has datafy representation, check if it's a collection.
    ;; If so, additionally datafy its items or map values.
    (let [datafied (case (object-type datafied)
                     :map (datafy-kvs datafied datafied true)
                     (:list :set) (datafy-seq datafied true)
                     datafied)]
      ;; Only render the datafy section if the datafied version of the object is
      ;; different than object, since we don't want to show the same data twice.
      (when-not (identical? datafied value)
        [datafied false]))

    (when (pageable? value)
      ;; If the value is a type that can be paged, then only datafy the
      ;; displayed chunk.
      (let [chunk (or chunk value)
            datafied (if (= (object-type value) :map)
                       (datafy-kvs value chunk false)
                       (datafy-seq chunk false))]
        (when datafied
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
            (render-items datafied {:map? (map? datafied)
                                    :start-idx start-idx
                                    :skip-nils? true})
            (render-trailing-page-ellipsis))
        ;; Otherwise, render datafied representation as a collection if it is
        ;; small enough, or as a single value.
        (render-value-maybe-expand ins datafied))

      (unindent ins))
    inspector))

;; Hex view mode

(defn- byte->ascii [b]
  (let [c (bit-and b 0xFF)]
    (if (and (>= c 32) (<= c 126))
      (char c)
      ;; Use MIDDLE DOT for non-printed chars as it is distinct from 0x2E.
      \·)))

(defn- format-hex-row
  "Format 16 bytes as hex values."
  [bytes]
  (let [hex-strs (mapv #(format "%02x" (bit-and % 0xFF)) bytes)
        padded (concat hex-strs (repeat (- 16 (count bytes)) "  "))
        [left-half right-half] (split-at 8 padded)]
    (str (str/join " " left-half) "  " (str/join " " right-half))))

(defn format-ascii-row
  "Format 16 bytes as ASCII characters."
  [bytes]
  (str/join (map byte->ascii bytes)))

(defn render-hexdump
  "Render the current array or array chunk as a hexdump-style table."
  [{:keys [value chunk start-idx] :as inspector}]
  (let [start-idx (or start-idx 0)
        lines (eduction (comp (partition-all 16)
                              (map-indexed vector))
                        (or chunk value))]
    (as-> inspector ins
      (render-leading-page-ellipsis ins)
      (reduce (fn [ins [i line]]
                (let [addr (+ (* i 16) start-idx)]
                  (render-indent-ln
                   ins (format "0x%08x │ %s │ %s" addr (format-hex-row line)
                               (format-ascii-row line)))))
              ins lines)
      (render-trailing-page-ellipsis ins))))

;; Inspector multimethod
(defn- dispatch-inspect [{:keys [view-mode] :as _ins} obj]
  (if (= view-mode :object)
    ;; When :object mode is requested, render all values as unrecognized objects.
    :default
    (object-type obj)))

(defmulti inspect #'dispatch-inspect)

(defmethod inspect :nil [inspector _obj]
  (-> inspector
      (render "Value: nil")
      (render-ln)
      (render-section-header "Contents")
      (indent)
      (render-indent-ln
       (rand-nth ["You have gazed into the void and it winked back."
                  "You've reached the end of the universe. Time to turn back."
                  "There's nothing here… or is there? Nope. Still nothing."
                  "Welcome to Nil. Population: 0."
                  "I sometimes come here too, to enjoy the peace and quiet."
                  "Here lies no data. Rest in peace, little bytes."
                  "Warning: staring too long at nil may summon cosmic horrors."
                  "This whole trip might have been for nothing! Zero, zilch, zip, nada, nothing."
                  "No data found. Please insert meaning manually."]))
      (unindent)))

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
  (as-> (render-class-name inspector obj) ins
    (render-counted-length ins obj)
    (render-labeled-value ins "Component Type" (.getComponentType (class obj)))
    (render-analytics ins)
    (render-section-header ins "Contents")
    (indent ins)
    (if (= (:view-mode inspector) :hex)
      (render-hexdump ins)
      (render-collection-paged ins))
    (unindent ins)
    (render-datafy ins)
    (render-page-info ins)))

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
      (render-indent-ln "Length: " (str (.length obj)))
      (render-section-header "Print")
      (indent)
      (render-indent-str-lines obj)
      (unindent)))

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
                             (let [static? (Modifier/isStatic (.getModifiers f))]
                               {:name (symbol (.getName f))
                                :value (compat/get-field-value f (when-not static? obj))
                                :static static?}))
                           all-fields)
        {static-accessible        [true true]
         non-static-accessible    [false true]
         static-nonaccessible     [true false]
         non-static-nonaccessible [false false]}
        (group-by (fn [{:keys [static value]}]
                    ;; Be careful to use identical? instead of = because an
                    ;; object might not implement equiv().
                    [static (not (identical? ::compat/access-denied value))])
                  field-values)
        ;; This is fine like this for now. If this condp ever grows bigger,
        ;; consider refactoring it into something polymorphic.
        printed (cond
                  (instance? Constructor obj)
                  (shorten-member-string (str obj) (.getDeclaringClass ^Constructor obj))

                  (instance? Method obj)
                  (shorten-member-string (str obj) (.getDeclaringClass ^Method obj))

                  (instance? Field obj)
                  (shorten-member-string (str obj) (.getDeclaringClass ^Field obj))

                  ;; Using print-str and not pprint intentionally, so that the
                  ;; `Value:` remains on a single line.
                  :else (print/print-str obj))]
    (letfn [(render-fields [inspector section-name field-values]
              (if (seq field-values)
                (-> inspector
                    (render-section-header section-name)
                    (indent)
                    (render-map-values
                     (->> field-values
                          (map (fn [{:keys [name value]}]
                                 [name
                                  (if (identical? value ::compat/access-denied)
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
                    (render "Identity hash code: " (str code) " "
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
      (render-items ins (.getStackTrace root-cause) {})
      (unindent ins)
      (render-datafy ins))))

(defmethod inspect :stack-trace-element [inspector ^StackTraceElement obj]
  (-> inspector
      (render-labeled-value "Class" (class obj))
      (render-section-header "Contents")
      (indent)
      (render-items (StackTraceElement->vec obj) {})))

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
          (render-indent-ln (str/join " " (:path inspector)))
          (unindent))
      inspector)))

(defn render-view-mode [{:keys [value view-mode pretty-print] :as inspector}]
  (if (some? value)
    (let [supported (filter #(view-mode-supported? inspector %) view-mode-order)
          add-circle #(if %2 (str "●" %1) %1)
          view-mode-str (str (->> supported
                                  (map #(add-circle (name %) (= % view-mode)))
                                  (str/join " "))
                             " " (add-circle "pretty" pretty-print))]
      (-> (render-section-header inspector "View mode (press 'v' to cycle, 'P' to pretty-print)")
          (indent)
          (render-indent view-mode-str)
          (unindent)))
    inspector))

(defn- finalize-rendered [rendered]
  (seq (persistent! rendered)))

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
         (update :rendered finalize-rendered)))))

;; Public entrypoints

(defn start
  "Create a new inspector for the `value`. Optinally accepts a `config` map (which
  can be an existing inspector with changed config). See `refresh` for the list
  of supported keys."
  ([value] (start {} value))
  ([config value]
   (let [inspector (-> default-inspector-config
                       (merge (validate-config config))
                       (assoc :stack [], :path [], :pages-stack [], :current-page 0,
                              :view-modes-stack [], :value value))]
     (-> inspector
         (assoc :view-mode (first (supported-view-modes inspector)))
         inspect-render))))

(defn ^:deprecated clear
  "If necessary, use `(start inspector nil) instead.`"
  [inspector]
  (start inspector nil))

(defn ^:deprecated fresh
  "If necessary, use `(start nil)` instead."
  []
  (start nil))
