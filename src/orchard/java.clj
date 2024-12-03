(ns orchard.java
  "Info for Java classes and members"
  {:author "Jeff Valk"}
  (:require
   [clojure.java.io :as io]
   [clojure.java.javadoc :as javadoc]
   [clojure.reflect :as reflect]
   [clojure.string :as string]
   [orchard.java.resource :as resource]
   [orchard.misc :as misc]
   [orchard.util.io :as util.io])
  (:import
   (clojure.lang IPersistentMap)
   (clojure.reflect Constructor Field JavaReflector Method)
   (java.net JarURLConnection)
   (java.util Map)
   (mx.cider.orchard LruMap)))

;;; ## Java Class/Member Info
;;
;; Getting class and member info (i.e. type hierarchy, method names,
;; argument/return types, etc) is straightforward using reflection; this
;; provides the basis of Java metadata support. When the source is available
;; (and a Java parser is too), we supplement reflection with source analysis to
;; get file, line, and column info, as well as docstrings and argument names.

;;; ## Classpath
;;
;; Java source files are resolved from the classpath. For library dependencies,
;; this simply entails having the corresponding source artifacts in the
;; project's dev dependencies. The core Java API classes are the exception to
;; this, since these are external to lein/maven dependency management. Hence,
;; JDK sources have to be put onto the classpath explicitly (e.g., manually or
;; with `enrich-classpath`).

(defn ^:deprecated jdk-find
  "Search common JDK path configurations for a specified file name and return a
  URL if found. This accommodates `java.home` being set to either the JDK root
  (JDK11+) or a JRE directory within this (JDK 8), and searches both the home and
  `lib` directories."
  [_]
  nil)

(def ^:deprecated jdk-sources nil)

(def jdk-tools
  "The `tools.jar` path, for JDK8 and earlier. If found on the existing
  classpath, this is the corresponding classpath entry. Otherwise, if available,
  this is added to the classpath."
  (when-not (>= misc/java-api-version 9)
    (some-> (io/resource "com/sun/javadoc/Doc.class")
            ^JarURLConnection (. openConnection)
            (. getJarFileURL))))

(defn ^:deprecated ensure-jdk-sources
  [])

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
  (delay
    (when (>= misc/java-api-version 11)
      (try (let [f (misc/require-and-resolve 'orchard.java.parser-next/source-info)]
             ;; We try parsing LruMap.java as a litmus test for whether the
             ;; parser works. We can be sure that LruMap.java is on the
             ;; classpath because we pack it into the final Orchard jar.
             (when (f `LruMap)
               f))
           (catch Throwable e
             (reset! parser-exception e)
             nil)))))

(defn source-info
  "Try to return class info from its parsed source if the source is available.
  Returns nil in case of any errors."
  [class-symbol]
  (try
    (when-let [f @parser-next-source-info]
      (f class-symbol))
    (catch Throwable e
      (reset! parser-exception e)
      (when (= (System/getProperty "orchard.internal.test-suite-running") "true")
        (throw e)))))

;; As of Java 11, Javadoc URLs begin with the module name.
(defn module-name
  "On JDK11+, return module name from the class if present; otherwise return nil"
  [klass]
  (when (>= misc/java-api-version 9)
    ((misc/require-and-resolve 'orchard.java.modules/module-name) klass)))

(defn javadoc-url
  "Return the relative `.html` javadoc path and member fragment."
  ([class]
   (let [maybe-module (when (>= misc/java-api-version 11)
                        (some-> (module-name class) (str "/")))]
     (str maybe-module
          (-> (string/replace (str class) "." "/")
              (string/replace "$" "."))
          ".html")))
  ([class member argtypes]
   (str (javadoc-url class) "#" member
        (when argtypes
          (if (<= misc/java-api-version 9) ; argtypes were munged before Java 10
            (str "-" (string/join "-" (map #(string/replace % #"\[\]" ":A") argtypes)) "-")
            (str "(" (string/join "," argtypes) ")"))))))

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
                 (string/replace "$" ".")
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
                                      (string/replace package "." "\\.")
                                      "\\.")))
        shorten (fn [s]
                  (cond-> s
                    package (string/replace package-re "")
                    true (string/replace #"^java\.lang\." "")))
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
                        (Class/forName (str class)) ;; NOTE: we don't pass the `false` argumnt since that complicates the analysis for deftype/defrecord classes.
                        (catch Exception _)
                        (catch NoClassDefFoundError _)
                        (catch LinkageError _))]
    (let [package (some-> c package symbol)
          {:keys [members] :as result} (misc/deep-merge (reflect-info (reflection-for c))
                                                        (when *analyze-sources*
                                                          (source-info class))
                                                        {:name       (-> c .getSimpleName symbol)
                                                         :class      (-> c .getName symbol)
                                                         :package    package
                                                         :super      (-> c .getSuperclass typesym)
                                                         :interfaces (map typesym (.getInterfaces c))
                                                         :javadoc    (javadoc-url class)})]
      (assoc result
             :members (into {}
                            (map (fn [[method arities]]
                                   [method (into {}
                                                 (map (fn [[k arity]]
                                                        [k (let [static? (:static (:modifiers arity))]
                                                             (-> arity
                                                                 (assoc :annotated-arglists
                                                                        (extract-annotated-arglists static? package arity))
                                                                 (dissoc :non-generic-argtypes)))]))
                                                 arities)]))
                            members)))))

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
        info (if cached
               (:info cached)
               (class-info* class))
        resource (:resource-url info)
        last-modified (if (or (nil? resource)
                              (util.io/url-to-file-within-archive? resource))
                        0
                        (util.io/last-modified-time resource))
        stale (not= last-modified (:last-modified cached))
        ;; If last-modified in cache mismatches last-modified of the file,
        ;; regenerate class-info.
        info (if (and cached stale)
               (class-info* class)
               info)]
    (when (and
           ;; Only cache full values that possibly were slowly computed.
           ;; It would be a mistake to cache the fast values, letting them shadow a full computation:
           *analyze-sources*
           (or (not cached) stale))
      (.put cache class {:info info, :last-modified last-modified}))
    info))

;;; ## Class/Member Info
;;
;; These functions filter the class info assembled above to respond to a more
;; specific query: type information for a class name, and member information for
;; a class/member combination.

(defn type-info
  "For the class or interface symbol, return Java type info. If the type has
  defined constructors, the line and column returned will be for the first of
  these for more convenient `jump` navigation."
  [class]
  (let [info (class-info class)
        ctor (->> (get-in info [:members class])
                  (vals)
                  (sort-by :line)
                  (filter :line)
                  (first))]
    (merge (dissoc info :members)
           (select-keys ctor [:line :column]))))

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

(defn trim-one-dot
  [s]
  (string/replace s #"^\.|\.$" ""))

(defn resolve-symbol
  "Return the info map for a Java member symbol.

  Constructors and static calls are resolved to the class
  unambiguously. Instance members are resolved unambiguously if defined
  by only one imported class. If multiple imported classes have a member
  by that name, a map of class names to member info is returned as
  `:candidates`."
  [ns sym]
  {:pre [(every? symbol? [ns sym])]}
  (let [sym (-> sym str trim-one-dot)
        sym* (symbol sym)
        [class static-member] (->> (string/split sym #"/" 2)
                                   (map #(when % (symbol %))))]
    (if-let [c (resolve-class ns class)]
      (when static-member
        (member-info (:class c) static-member))     ; SomeClass/methodCall
      (when-let [ms (seq (resolve-member ns sym*))] ; methodCall
        (if (= 1 (count ms))
          (first ms)
          {:candidates (zipmap (map :class ms) ms)})))))

(defn resolve-type
  "Return type info, for a Java class, interface or record."
  [ns sym]
  (let [sym (-> sym str trim-one-dot)
        sym-split (->> (string/split sym #"/" 2)
                       (map #(when % (symbol %))))]
    (some->> (first sym-split)
             (resolve-class ns)
             :class
             type-info)))

(defn javadoc-base-url
  "Re-implementation of `clojure.java.javadoc/*core-java-api*` because it doesn't
  contain newer JDK versions, especially in older Clojure."
  [jdk-version]
  (cond (<= jdk-version 10)
        (format "https://docs.oracle.com/javase/%s/docs/api/" jdk-version)
        (<= 11 jdk-version 22)
        (format "https://docs.oracle.com/en/java/javase/%s/docs/api/" jdk-version)
        :else ;; For newer JDK version, default to latest LTS.
        (recur 21)))

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

(defn- initialize-cache!* []
  (doseq [class [`Thread `String 'java.io.File]]
    (class-info class)))

(def initialize-cache-silently?
  "Should `#'cache-initializer` refrain from printing to `System/out`?"
  (= "true" (System/getProperty "orchard.initialize-cache.silent" "true")))

(def ^:private initialize-cache!
  (cond-> initialize-cache!*
    initialize-cache-silently? util.io/wrap-silently))

(def cache-initializer
  "Cache info for a few classes.
  This also warms up the cache for some underlying, commonly needed classes (e.g. `Object`).

  This is a def for allowing others to wait for this workload to complete (can be useful sometimes)."
  (delay ;; NOTE: this used to be a `future`, but that can cause odd issues.
    (initialize-cache!)))
