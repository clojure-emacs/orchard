(ns orchard.java
  "Info for Java classes and members"
  {:author "Jeff Valk"}
  (:require
   [clojure.java.javadoc :as javadoc]
   [clojure.reflect :as reflect]
   [clojure.string :as str]
   [orchard.java.compatibility :as compat]
   [orchard.java.resource :as resource]
   [orchard.java.source-files :as src-files]
   [orchard.misc :as misc]
   [orchard.util.io :as util.io])
  (:import
   (clojure.lang IPersistentMap)
   (clojure.reflect Constructor Field JavaReflector Method)
   (java.util Map)
   (mx.cider.orchard LruMap)))

;;; ## Java Class/Member Info
;;
;; Getting class and member info (i.e. type hierarchy, method names,
;; argument/return types, etc) is straightforward using reflection; this
;; provides the basis of Java metadata support. When the source is available (on
;; JDK11+), we supplement reflection with source analysis to get file, line, and
;; column info, as well as docstrings and argument names.
;;
;; Java source files can be resolved either from the classpath, from JDK
;; distribution directory, on from Maven source dependencies. See
;; `orchard.java.source-files` for the details.

;;; ## Javadoc URLs
;;
;; Relative Javadoc URLs can be constructed from class/member signatures.
;;
;; N.b. Where a method's bytecode signature differs from its declared signature
;; (other than collection generics), the javadoc method URL can't be inferred as
;; done below. (Specifically, this applies to varargs and non-collection
;; generics, e.g. `java/misc/Arrays.html#asList(T...)`.) Since the member is
;; just a URL fragment, the javadoc link will simply navigate to the parent
;; class in these cases.

;;; ## Source Analysis
;;
;; Java parser support is available for JDK11+ via `java.parser-next`.

(def parser-exception
  "The exception found, if any, when running any parser."
  (atom nil))

(def ^:private parser-next-source-info
  (when (>= misc/java-api-version 11)
    (requiring-resolve 'orchard.java.parser-next/source-info)))

(defn source-info
  "Try to return class info from its parsed source if the source is available.
  Returns nil in case of any errors."
  ([class-symbol]
   ;; Arity for backward compatibility.
   (when parser-next-source-info
     (parser-next-source-info class-symbol)))
  ([klass source-url]
   (when parser-next-source-info
     (parser-next-source-info klass source-url))))

(defn javadoc-url
  "Return the relative `.html` javadoc path and member fragment."
  ([class]
   (let [url (str (-> (str/replace (str class) "." "/")
                      (str/replace "$" "."))
                  ".html")
         ;; As of Java 11, Javadoc URLs begin with the module name.
         module (compat/module-name class)]
     (cond->> url
       module (format "%s/%s" module))))
  ([class member argtypes]
   (str (javadoc-url class) "#" member
        (when argtypes
          (if (< misc/java-api-version 11) ; argtypes were munged before Java 11
            (str "-" (str/join "-" (map #(str/replace % #"\[\]" ":A") argtypes)) "-")
            (str "(" (str/join "," argtypes) ")"))))))

;;; ## Class Metadata Assembly
;;
;; We construct metadata at the class level, first using `reflect-info` to
;; transform the metadata returned by `clojure.reflect/reflect`. This is then
;; merged with a source analysis pass (when available) from `source-info`. The
;; nested map structure and keys returned by these two functions is identical
;; for the same class. Class members are indexed first by name, then argument
;; types.

(defn typesym
  [o]
  (some-> o reflect/typename symbol))

(defprotocol Reflected
  (reflect-info [o]))

(defn- format-like-parser-next [argtypes]
  ;; make the format match with that of `parser-next`:
  (mapv #(some-> %
                 str
                 (str/replace "$" ".")
                 symbol
                 typesym)
        argtypes))

(extend-protocol Reflected
  Constructor
  (reflect-info [c]
    (let [argtypes (mapv typesym (:parameter-types c))]
      {:argtypes argtypes
       :non-generic-argtypes (format-like-parser-next argtypes)
       :throws (mapv typesym (:exception-types c))}))

  Method
  (reflect-info [m]
    (let [pts (:parameter-types m)
          argtypes (mapv typesym pts)]
      {:argtypes argtypes
       :non-generic-argtypes (format-like-parser-next argtypes)
       :parameter-types pts
       :throws (mapv typesym (:exception-types m))
       :returns (typesym (:return-type m))}))

  Field
  (reflect-info [f]
    {:type (typesym (:type f))})

  IPersistentMap ; => Class
  (reflect-info [c]
    (let [map-members (->> c
                           :members
                           ;; removes:
                           ;; * compareTo from the interface (only the one from the class itself is relevant)
                           ;; * lambda$indent$0 (lambda stuff)
                           (remove (some-fn (comp (partial some #{:synthetic}) :flags)
                                            (fn [{member-name :name}]
                                              ;; Removes $$YJP$$sleep (yourkit stuff)
                                              (some-> member-name str (.contains "$$YJP$$"))))))
          members (->> map-members
                       ;; Merge type-specific attributes with common ones.
                       (map (fn [m]
                              (merge {:name (:name m)
                                      :modifiers (:flags m)}
                                     (reflect-info m))))
                       ;; Index by name, argtypes. Args for fields are nil.
                       (group-by :name)
                       (reduce (fn [ret [n ms]]
                                 (assoc ret n (zipmap (map :non-generic-argtypes ms) ms)))
                               {}))
          class-name (or (:name c)
                         (some :declaring-class map-members))
          class-name (if (some-> class-name class?)
                       (-> ^Class class-name .getName symbol)
                       class-name)]
      {:name class-name
       :modifiers (:flags c)
       :members members})))

(defn- package
  "An alternative to .getPackage, which works for classes defined with deftype and defrecord.
  See https://dev.clojure.org/jira/browse/CLJ-1550 for details."
  [^Class kls]
  (if-let [package (some-> kls .getPackage .getName)]
    package
    ;; that workaround is needed only on Java 8
    (let [kls (.getName kls)
          idx (.lastIndexOf kls ".")]
      (when (pos? idx)
        (subs kls 0 idx)))))

(defn +this [xs]
  (into ['this] xs))

(defn extract-arglist [static? xs]
  ((if static? identity +this)
   (or (:argnames xs) (:argtypes xs))))

(defn extract-parameter-type [static? xs]
  ((if static? identity +this)
   (or (:non-generic-argtypes xs)
       (:parameter-types xs))))

(defn extract-annotated-arglists [static? package {:keys [returns] :as x}]
  (let [arglist (extract-arglist static? x)
        parameter-type (extract-parameter-type static? x)
        sb (StringBuilder.)
        package-re (when package
                     (re-pattern (str "^"
                                      (str/replace package "." "\\.")
                                      "\\.")))
        shorten (fn [s]
                  (cond-> s
                    package (str/replace package-re "")
                    true (str/replace #"^java\.lang\." "")))
        fill-arglist!
        (fn []
          (into []
                (map-indexed (fn [n i]
                               (when-not (zero? n)
                                 (.append sb \,)
                                 (.append sb \space))
                               (let [i-str (str i)]
                                 (when-not (.equals i-str "this")
                                   (when-let [m (some-> parameter-type
                                                        (get n) ;; can occasionally return nil
                                                        str
                                                        shorten)]
                                     (.append sb \^)
                                     (.append sb m)
                                     (.append sb \space)))
                                 (.append sb i-str))))
                arglist))]
    (when-let [m (some-> returns
                         str
                         shorten)]
      (.append sb \^)
      (.append sb m)
      (.append sb \space))
    (.append sb \[)
    (fill-arglist!)
    (.append sb \])
    (str sb)))

(defn- reflection-for [^Class c]
  (reflect/reflect c
                   ;; for dynamically loaded classes:
                   :reflector (JavaReflector. (.getClassLoader c))))

(def ^:dynamic *analyze-sources*
  "Whether to analyze .java sources in addition to reflection-gathered info.

  Bind this to `false` in order to increase performance / decrease the amount of information returned."
  true)

(defn class-info*
  "For the class symbol, return Java class and member info. Members are indexed
  first by name, and then by argument types to list all overloads."
  [class]
  (when-let [^Class c (try
                        ;; NOTE: we don't pass the `false` argument since that
                        ;; complicates the analysis for deftype/defrecord
                        ;; classes.
                        (Class/forName (str class))
                        (catch Exception _)
                        (catch NoClassDefFoundError _)
                        (catch LinkageError _))]
    (let [package (some-> c package symbol)
          relative-source-path (src-files/class->sourcefile-path c)
          source-file-url (src-files/class->source-file-url c)
          result (misc/deep-merge (reflect-info (reflection-for c))
                                  (when source-file-url
                                    ;; :file is the original historic key.
                                    ;; :file-url was added later and is used by
                                    ;; CIDER more now. Both have the same value
                                    ;; for compatibility.
                                    {:file     source-file-url
                                     :file-url source-file-url
                                     :resource relative-source-path})
                                  (when (and *analyze-sources* source-file-url)
                                    (try (source-info c source-file-url)
                                         (catch Exception _)))
                                  {:name       (-> c .getSimpleName symbol)
                                   :class      (-> c .getName symbol)
                                   :package    package
                                   :super      (-> c .getSuperclass typesym)
                                   :interfaces (map typesym (.getInterfaces c))
                                   :javadoc    (javadoc-url class)})]
      (update result :members
              (fn [members]
                (misc/update-vals
                 (fn [arities]
                   (misc/update-vals
                    #(let [static? (:static (:modifiers %))]
                       (-> %
                           (assoc :annotated-arglists
                                  (extract-annotated-arglists static? package %))
                           (dissoc :non-generic-argtypes)))
                    arities))
                 members))))))

#_(class-info* `Thread)
#_(class-info* 'clojure.lang.PersistentList)
#_(class-info* 'mx.cider.orchard.LruMap)

;;; ## Class Metadata Caching
;;
;; When it won't change, cache the class info. Otherwise when we analyze
;; hundreds or more classes at once (as with a naive symbol resolution call),
;; duplicated reflection and source parsing becomes a wasteful performance hit.
;;
;; To support mixed Clojure/Java projects where `.java` files are being updated
;; and recompiled, we cache such classes with last-modified property, so that we
;; know when to purge those classes from cache.
;;
;; We chose to implement the custom `LruMap` mechanism so that
;; Orchard can remain a dependency-free project.
;;
;; The cache size of 250 is large enough to be useful (and hold a few key classes, like Object),
;; and small enough to not incur into OOMs.
(def ^Map cache (LruMap. 250))

(defn class-info
  "For the class symbol, return (possibly cached) Java class and member info.
  Members are indexed first by name, and then by argument types to list all
  overloads."
  [class]
  (let [cached (.get cache class)
        file-url (-> cached :info :file-url)
        last-modified (some-> file-url util.io/last-modified-time)
        ;; Cache is valid only if the cached info discovered a valid file-url,
        ;; and the modified date of that file matches the remembered date.
        cache-valid? (and file-url (= last-modified (:last-modified cached)))]
    (if cache-valid?
      (:info cached)

      (let [{:keys [file-url] :as info} (class-info* class)]
        ;; Only cache value if we discovered the source file and analyzed it.
        (when (and *analyze-sources* file-url)
          (.put cache class
                {:info info
                 :last-modified (util.io/last-modified-time file-url)}))
        info))))

;;; ## Class/Member Info
;;
;; These functions filter the class info assembled above to respond to a more
;; specific query: type information for a class name, and member information for
;; a class/member combination.

(defn member-info
  "For the class and member symbols, return Java member info. If the member is
  overloaded, line number and javadoc signature are that of the first overload.
  If the member's definition is in a superclass, info returned will be for the
  implementation. If the member is an instance member, `this` is prepended to its
  arglists."
  [class member]
  (let [c
        ;; NOTE: the following code uses `loop` to avoid retaining more objects in memory than necessary.
        ;; `class-info` calls can be expensive given the JavaDoc parsing they can perform.
        (loop [next-class class]
          (let [{:keys [super] :as c-i} (class-info next-class)
                v (get-in c-i [:members member])]
            (cond
              v c-i
              (not super) nil
              :else (recur super))))]
    (when-let [m (get-in c [:members member])]
      (let [siblings (sort-by :line (vals m))
            m* (first siblings)
            static? (or (:static (:modifiers m*))
                        (= class member))]
        (-> (dissoc m* :name :argnames)
            (assoc :class class
                   :member member
                   :file (:file c)
                   :file-url (:file-url c)
                   :annotated-arglists (mapv :annotated-arglists siblings)
                   :arglists (mapv (partial extract-arglist static?)
                                   siblings)
                   :javadoc (javadoc-url class member
                                         (:argtypes m*))))))))

;;; ## Class/Member Resolution
;;
;; A namespace provides a search context for resolving a symbol to a Java class
;; or member. Classes, constructors, and static members can be resolved
;; unambiguously. With instance members, more than one imported class may have
;; a member that matches the searched symbol. In such cases, the result will be
;; a list of resolved candidate members. (Note that this list could be narrowed
;; by considering arity, and narrowed further *if* we could consider argument
;; types...)

(defn resolve-class
  "Given namespace and class symbols, search the imported classes and return
  class info. If not found, search all classes on the classpath (requires a
  qualified name)."
  [ns sym]
  (when-let [ns (find-ns ns)]
    (let [c (try (ns-resolve ns sym)
                 (catch java.lang.NoClassDefFoundError _)
                 (catch Exception _))]
      (if (class? c)
        (class-info (-> ^Class c .getName symbol))
        (class-info sym)))))

(defn resolve-member
  "Given namespace and member symbols, search the imported classes and return
  a list of each matching member's info."
  [ns sym]
  (when-let [ns (find-ns ns)]
    (->> (vals (ns-imports ns))
         (keep #(member-info (-> ^Class % .getName symbol) sym))
         (distinct))))

(defn resolve-constructor
  "Given namespace and classname symbols, search the first constructor for the
  given class and return its info."
  [ns class-sym]
  (when-let [info (resolve-class ns class-sym)]
    (when-let [ctors (->> (get-in info [:members (:class info)])
                          vals
                          (sort-by :line)
                          seq)]
      (merge (dissoc info :members)
             (select-keys (some #(when (:line %) %) ctors)
                          [:line :column])))))

(defn resolve-symbol
  "Return the info map for a Java member symbol. The following Java symbols are
  supported:
  - Java classes (`Thread` and `java.lang.Thread`)
  - Java classes with module prefix (`java.base/java.lang.Thread`)
  - constructors (`Thread.` and `java.lang.Thread.`)
  - static members (`Thread/currentThread`)
  - instance members for classes imported into `ns` (`.start`)
  - qualified instance members (`Thread/.start`)
  - Java-style printed member references (`clojure.lang.AFn.run`)

  If multiple imported classes have a non-qualified instance member by that
  name, a map of class names to member info is returned as `:candidates`."
  [ns sym]
  {:pre [(every? symbol? [ns sym])]}
  (let [s (str sym)]
    (or (when-let [[_ klass] (re-matches #"(.+)\." s)]
          (resolve-constructor ns (symbol klass)))

        (resolve-class ns (symbol s)) ;; When s is a class symbol

        (when-let [[_ instance-member] (re-matches #"\.(.+)" s)]
          (let [ms (->> (resolve-member ns (symbol instance-member))
                        (remove #(:static (:modifiers %))))]
            (condp = (count ms)
              0 nil
              1 (first ms)
              {:candidates (zipmap (map :class ms) ms)})))

        (when-let [[_ klass member] (re-matches #"(.+)/\.?([^/]+)" s)]
          (when-let [c (resolve-class ns (symbol klass))]
            (member-info (:class c) (symbol member))))

        ;; Special case: java classes with module prefix.
        (when-let [[_ klass] (re-matches #"[^/]+/([^/]+)" s)]
          (resolve-class ns (symbol klass)))

        ;; Special case: java methods that are printed in stacktraces and look
        ;; like this: clojure.lang.AFn.run or java.base/java.lang.Thread.run
        (when-let [[_ klass member] (re-matches #"(?:[^/]+/)?(.+)\.([^\.]+)" s)]
          (when-let [c (resolve-class ns (symbol klass))]
            (member-info (:class c) (symbol member)))))))

;;;; Online Javadoc

(defn javadoc-base-url
  "Re-implementation of `clojure.java.javadoc/*core-java-api*` because it doesn't
  contain newer JDK versions, especially in older Clojure."
  [jdk-version]
  (if (< jdk-version 11)
    (format "https://docs.oracle.com/javase/%s/docs/api/" jdk-version)
    (format "https://docs.oracle.com/en/java/javase/%s/docs/api/" jdk-version)))

(defn resolve-javadoc-path
  "Resolve a relative javadoc path to a URL and return as a map. Prefer javadoc
  resources on the classpath; then use online javadoc content for core API
  classes. If no source is available, return the relative path as is."
  [^String path]
  (or (resource/resource-full-path path)
      (some (let [classname (.replaceAll path "/" ".")]
              (fn [[prefix url]]
                (when (.startsWith classname prefix)
                  (str url path))))
            (into @javadoc/*remote-javadocs*
                  ;; Older Clojure versions don't have javadoc for newer JDKs.
                  ;; We just backport them regardless of Clojure version.
                  (zipmap ["java." "javax." "org.ietf.jgss." "org.omg." "org.w3c.dom." "org.xml.sax"]
                          (repeat (javadoc-base-url misc/java-api-version)))))
      path))
