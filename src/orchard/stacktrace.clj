(ns orchard.stacktrace
  "Instruments for analyzing exceptions and stacktraces which process exception
  objects and attach extra data to them."
  {:added "0.31"
   :author "Jeff Valk, Oleksandr Yakushev"}
  (:refer-clojure :exclude [print-str])
  (:require
   [clojure.java.io :as io]
   [clojure.main]
   [clojure.repl :as repl]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [orchard.info :as info]
   [orchard.java.resource :as resource]
   [orchard.misc :as misc :refer [assoc-some]]
   [orchard.print :as print])
  (:import
   (java.net URL)
   (java.nio.file Path)))

(def ^:private ^Path cwd-path (.toAbsolutePath (.toPath (io/file ""))))

(defn- print-str [value]
  ;; Limit printed collections to 5 items.
  (binding [*print-length* 5]
    (print/print-str value)))

;;; ## Stacktraces

(defn- Throwable->map-with-traces
  "Like `Throwable->map` but attaches `:trace` key to all causes in `:via`."
  [^Throwable o]
  (let [m (Throwable->map o)
        causes (take-while some? (iterate #(.getCause ^Throwable %) o))]
    (update m :via
            (fn [via]
              (mapv (fn [v, ^Throwable t]
                      (let [st (.getStackTrace t)]
                        (if (pos? (alength st))
                          (assoc v :trace (mapv StackTraceElement->vec st))
                          v)))
                    via causes)))))

;; Java stacktraces don't expose column number.
(defn- frame-tuple->map
  "Return a map describing the stack frame."
  [frame]
  (let [[class method file line] frame]
    (when (and class method file line)
      {:name   (str (name class) "/" (name method))
       :file   file
       :line   line
       :class  (name class)
       :method (name method)})))

(defn- flag-frame
  "Update frame's flags vector to include the new flag."
  [frame flag]
  (update frame :flags (comp set conj) flag))

(defn- path->url
  "Return a url for the path, either relative to classpath, or absolute."
  [path]
  (or (info/file-path path) (second (resource/resource-path-tuple path))))

(defn- infer-clojure-source-file [munged-class-name]
  (let [path-wo-ext (-> munged-class-name
                        (str/replace #"\$.*" "")
                        (str/replace "." "/"))]
    (or (io/resource (str path-wo-ext ".clj"))
        (io/resource (str path-wo-ext ".cljc")))))

(defn- analyze-class
  "Add namespace, fn, and var to the frame map when the source is a Clojure
  function."
  [{:keys [type class method] :as frame}]
  (if (or (= :clj type)
          (= :cljc type))
    (let [[ns fn & anons] (-> (repl/demunge class)
                              (str/replace #"--\d+" "")
                              (str/split #"/"))
          fn (or fn method)]            ; protocol functions are not munged
      (assoc frame
             :ns ns
             :fn (str/join "/" (cons fn anons))
             :var (str ns "/" fn)
             ;; File URL on the classpath
             :file-url (infer-clojure-source-file class)))
    frame))

(defn- analyze-file
  "Associate the file type (extension) of the source file to the frame map, and
  add it as a flag. If the name is `NO_SOURCE_FILE`, type `clj` is assumed."
  [{:keys [file] :as frame}]
  (let [[_ ext] (some->> file (re-find #"\.([^\.]+)$"))
        type (cond (nil? file)               :unknown
                   (= file "NO_SOURCE_FILE") :clj
                   (str/blank? ext)          :unknown
                   :else                     (keyword ext))]
    (-> frame
        (assoc :type type)
        (flag-frame type))))

(defn- flag-repl
  "Flag the frame if its source is a REPL eval."
  [{:keys [file] :as frame}]
  (if (and file
           (or (= file "NO_SOURCE_FILE")
               (.startsWith ^String file "form-init")))
    (flag-frame frame :repl)
    frame))

(defn- flag-project
  "Flag the frame if it is from the user project. The heuristic is this: if we
  found the source file, it is a file on the filesystem (not in the JAR), and it
  resides in CWD — it is a project frame, otherwise it's a dependency frame."
  [{:keys [^URL file-url] :as frame}]
  (if file-url
    (-> frame
        (flag-frame (if (and (= (.getProtocol file-url) "file")
                             (-> file-url .getFile io/file .toPath
                                 (.startsWith cwd-path)))
                      :project :dependency))
        (update :file-url str)) ;; Stringify file-url for bencode transfer.
    ;; If file-url is absent, we can't flag it as neither.
    frame))

(defn- analyze-frame
  "Return the stacktrace as a sequence of maps, each describing a stack frame."
  [frame]
  (-> frame
      (frame-tuple->map)
      (analyze-file)
      (analyze-class)
      (flag-project)
      (flag-repl)))

(defn- flag-duplicates
  "Where a parent and child frame represent substantially the same source
  location, flag the parent as a duplicate."
  [frames]
  (->> frames
       (partition 2 1)
       (map (fn [[frame parent]]
              (if (or (= (:name frame) (:name parent))
                      (and (= (:file frame) (:file parent))
                           (= (:line frame) (:line parent))))
                (flag-frame parent :dup)
                parent)))
       (into [(first frames)])))

(def ^:private tooling-frame-re
  #"^clojure\.lang\.LazySeq|^clojure\.lang\.Var|^clojure\.lang\.MultiFn|^clojure\.lang\.AFn|^clojure\.lang\.RestFn|^clojure\.lang\.RT|clojure\.lang\.Compiler|^nrepl\.|^cider\.|^refactor-nrepl\.|^shadow.cljs\.|^clojure\.core/eval|^clojure\.core/apply|^clojure\.core/with-bindings|^clojure\.core\.protocols|^clojure\.core\.map/fn|^clojure\.core/binding-conveyor-fn|^clojure\.main/repl")

(defn- tooling-frame-name? [frame-name]
  (let [demunged (repl/demunge frame-name)]
    (boolean (re-find tooling-frame-re demunged))))

(defn- flag-tooling
  "Given a collection of stack `frames`, marks the 'tooling' ones as such.
  A 'tooling' frame is one that generally represents Clojure, JVM, nREPL or CIDER
  internals, and that is therefore not relevant to application-level code."
  [frames]
  ;; Iterate frames from the end. Mark all consecutive Thread-like frames as
  ;; tooling, and also all frames that match `tooling-frame-name?`.
  (loop [frames (vec frames), i (dec (count frames)), all-tooling-so-far? true]
    (if (< i 0)
      frames
      (let [frame-name (:name (get frames i))
            tooling? (and frame-name
                          (or (tooling-frame-name? frame-name)
                              ;; Everything runs from a Thread, so this frame,
                              ;; if at the end, is irrelevant. However one can
                              ;; invoke this method 'by hand', which is why we
                              ;; only skip consecutive frames that match this.
                              (and all-tooling-so-far?
                                   (re-find #"^java\.lang\.Thread/run|^java\.util\.concurrent"
                                            frame-name))))]
        (recur (cond-> frames
                 tooling? (update i flag-frame :tooling))
               (dec i) (and all-tooling-so-far? tooling?))))))

;;; ## Causes

(defn- relative-path
  "If the path is under the project root, return the relative path; otherwise
  return the original path."
  [path]
  (let [child-path (.toPath (io/file path))]
    (if (.startsWith child-path cwd-path)
      (str (.relativize cwd-path child-path))
      path)))

(defn- extract-location
  "If the cause is a compiler exception, extract the useful location information
  from `:location`. Include relative path for simpler reporting."
  [{:keys [class location] :as cause}]
  (if (and (= class "clojure.lang.Compiler$CompilerException") location)
    ;; Post-1.9, CompilerExceptions always carry location data.
    (assoc cause
           :file (:clojure.error/source location)
           :file-url (some-> (:clojure.error/source location)
                             path->url
                             str)
           :path (relative-path (:clojure.error/source location))
           :line (:clojure.error/line location)
           :column (:clojure.error/column location))
    cause))

;; CLJS REPLs use :repl-env to store huge amounts of analyzer/compiler state
(def ^:private ex-data-blocklist
  #{:repl-env})

(defn- filter-ex-data
  "Filter keys from the exception `data` which are blocklisted (generally for
  containing data not intended for reading by a human)."
  [data]
  (when data
    (into {} (remove #(ex-data-blocklist (key %))) data)))

(defn- prepare-spec-data
  "Prepare spec problems for display in user stacktraces. Take in a map `ed` as
  returned by `clojure.spec.alpha/explain-data` and return a map of printed
  problems. The content of the returned map is modeled after
  `clojure.spec.alpha/explain-printer`."
  [ed]
  (let [problems (sort-by #(count (:path %)) (::s/problems ed))]
    {:spec (pr-str (::s/spec ed))
     :value (print-str (::s/value ed))
     :problems
     (mapv
      (fn [{:keys [in val pred reason via path] :as prob}]
        (->> {:in (some-> in not-empty print-str)
              :val (print-str val)
              :predicate (pr-str (s/abbrev pred))
              :reason reason
              :spec (some-> via not-empty last pr-str)
              :at (some-> path not-empty pr-str)
              :extra
              (let [extras (into {}
                                 (remove #(#{:in :val :pred :reason :via :path
                                             ::s/failure} (key %)))
                                 prob)]
                (when (seq extras)
                  (print-str extras)))}
             (filter clojure.core/val)
             (into {})))
      problems)}))

(defn- analyze-stacktrace-data
  "Return the stacktrace as a sequence of maps, each describing a stack frame."
  [trace]
  (when (seq trace)
    (-> (misc/pmap analyze-frame trace)
        (flag-duplicates)
        (flag-tooling))))

(defn- analyze-cause
  "Analyze the `cause-data` of an exception, in `Throwable->map` format."
  [cause-data]
  (let [phase (-> cause-data :data :clojure.error/phase)
        m (-> {:class (name (:type cause-data))
               :phase phase
               :message (:message cause-data)
               :stacktrace (analyze-stacktrace-data
                            (cond (seq (:trace cause-data)) (:trace cause-data)
                                  (:at cause-data) [(:at cause-data)]))}
              (assoc-some :triage (:triage cause-data)))]
    (if-let [data (filter-ex-data (:data cause-data))]
      (if (::s/failure data)
        (assoc m
               :message "Spec assertion failed."
               :spec (prepare-spec-data data))
        (assoc m
               :data (print-str data)
               :location (select-keys data [:clojure.error/line
                                            :clojure.error/column
                                            :clojure.error/phase
                                            :clojure.error/source
                                            :clojure.error/symbol])))
      m)))

(defn- maybe-triage-message
  "If the exception is a compiler error which carries Spec-based explanation data,
  transform it into human readable error message string."
  [exception-data]
  (try
    ;; ex-triage may throw an exception if :phase is incorrect
    (when-let [explanation-data (:clojure.error/spec
                                 (clojure.main/ex-triage exception-data))]
      (with-out-str (s/explain-out explanation-data)))
    (catch Exception _)))

(defn- analyze-causes
  "Analyze the cause chain of the `exception-data` in `Throwable->map` format."
  [exception-data]
  (let [triage-message (maybe-triage-message exception-data)
        causes (update (vec (:via exception-data)) 0
                       #(cond-> %
                          ;; If the first cause lacks :trace, add :trace of the
                          ;; exception there.
                          (nil? (:trace %)) (assoc :trace (:trace exception-data))
                          ;; If non-nil, assoc triage-message to first cause.
                          triage-message (assoc :triage triage-message)))]
    (mapv #(extract-location (analyze-cause %)) causes)))

(defn analyze
  "Return the analyzed cause chain for `exception` beginning with the thrown
  exception. `exception` can be an instance of `Throwable` or a map in the same
  format as `Throwable->map`. For `ex-info` exceptions, the response contains a
  `:data` slot with the printed data. For clojure.spec asserts, the `:spec` slot
  contains a map of printed components describing spec failures."
  [exception]
  (cond (instance? Throwable exception)
        (analyze-causes (Throwable->map-with-traces exception))
        (and (map? exception) (:trace exception))
        (analyze-causes exception)))
