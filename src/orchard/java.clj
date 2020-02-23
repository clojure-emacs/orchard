(ns orchard.java
  "Info for Java classes and members"
  {:author "Jeff Valk"}
  (:require
   [clojure.java.io :as io]
   [clojure.java.javadoc :as javadoc]
   [clojure.reflect :as r]
   [clojure.string :as str]
   [orchard.java.classpath :as cp]
   [orchard.misc :as misc]
   [orchard.java.resource :as resource])
  (:import
   (clojure.lang IPersistentMap)
   (clojure.reflect Constructor Field JavaReflector Method)
   (java.io File)
   (java.net URI)))

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
;; this, since these are external to lein/maven dependency management. For
;; these, we search the JDK directory and add the source classpath entry
;; manually, if available. Prior to JDK9, parsing source files also requires
;; having `tools.jar` on the classpath, which we'll have to add as well.

(defn jdk-find
  "Search common JDK path configurations for a specified file name and return a
  URL if found. This accommodates `java.home` being set to either the JDK root
  (JDK9+) or a JRE directory within this (JDK 8), and searches both the home and
  `lib` directories."
  [f]
  (let [home (io/file (System/getProperty "java.home"))
        parent (.getParentFile home)
        paths [(io/file home f)
               (io/file home "lib" f)
               (io/file parent f)
               (io/file parent "lib" f)]]
    (->> paths (filter #(.canRead %)) first io/as-url)))

(def jdk-sources
  "The JDK sources path. If found on the existing classpath, this is the
  corresponding classpath entry. Otherwise, the JDK directory is searched for
  the file `src.zip`, and if found this added to the classpath."
  (let [base-url (fn [path]
                   (some-> (io/resource path)
                           (.. openConnection getJarFileURL)))]
    (or (base-url "java.base/java/lang/Object.java") ; JDK9+
        (base-url "java/lang/Object.java")           ; JDK8-
        (some-> (jdk-find "src.zip") cp/add-classpath!))))

(def jdk-tools
  "The `tools.jar` path, for JDK8 and earlier. If found on the existing
  classpath, this is the corresponding classpath entry. Otherwise, if available,
  this is added to the classpath."
  (when (<= misc/java-api-version 8)
    (or (some-> (io/resource "com/sun/javadoc/Doc.class")
                (.. openConnection getJarFileURL))
        (some-> (jdk-find "tools.jar") cp/add-classpath!))))

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

;; As of Java 11, Javadoc URLs begin with the module name.
(declare module-name)

(defn javadoc-url
  "Return the relative `.html` javadoc path and member fragment."
  ([class]
   (let [maybe-module (when (>= misc/java-api-version 11)
                        (some-> (module-name class) (str "/")))]
     (str maybe-module
          (-> (str/replace (str class) "." "/")
              (str/replace "$" "."))
          ".html")))
  ([class member argtypes]
   (str (javadoc-url class) "#" member
        (when argtypes
          (if (<= misc/java-api-version 9) ; argtypes were munged before Java 10
            (str "-" (str/join "-" (map #(str/replace % #"\[\]" ":A") argtypes)) "-")
            (str "(" (str/join "," argtypes) ")"))))))

;;; ## Source Analysis
;;
;; Java parser support is available for JDK9+ and JDK8 and below via separate
;; namespaces, `java.parser` and `java.legacy-parser`. The former uses only
;; external JDK APIs and supports modular (Jigsaw) sources. The latter uses
;; internal APIs out of necessity. Once this project discontinues support for
;; JDK8, the legacy parser may be removed.

(def source-info
  "When a Java parser is available, return class info from its parsed source;
  otherwise return nil."
  (if (>= misc/java-api-version 9)
    (do (require '[orchard.java.parser :as src])
        (resolve 'src/source-info))
    (if jdk-tools
      (do (require '[orchard.java.legacy-parser :as src])
          (resolve 'src/source-info))
      (constantly nil))))

(def module-name
  "On JDK9+, return module name from the class if present; otherwise return nil"
  ;; NOTE This function exists in the parser namespace for conditional
  ;; loading on JDK9+; it does not require parsing.
  (if (>= misc/java-api-version 9)
    (resolve 'src/module-name)
    (constantly nil)))

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
  (when o (symbol (r/typename o))))

(defprotocol Reflected
  (reflect-info [o]))

(extend-protocol Reflected
  Constructor
  (reflect-info [c]
    {:argtypes (mapv typesym (:parameter-types c))
     :throws (map typesym (:exception-types c))})

  Method
  (reflect-info [m]
    {:argtypes (mapv typesym (:parameter-types m))
     :throws (map typesym (:exception-types m))
     :returns (typesym (:return-type m))})

  Field
  (reflect-info [f]
    {:type (typesym (:type f))})

  IPersistentMap ; => Class
  (reflect-info [c]
    {:name (:name c)
     :modifiers (:flags c)
     :members (->> (:members c)
                   ;; Merge type-specific attributes with common ones.
                   (map (fn [m]
                          (merge {:name (:name m)
                                  :modifiers (:flags m)}
                                 (reflect-info m))))
                   ;; Index by name, argtypes. Args for fields are nil.
                   (group-by :name)
                   (reduce (fn [ret [n ms]]
                             (assoc ret n (zipmap (map :argtypes ms) ms)))
                           {}))}))

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

(defn class-info*
  "For the class symbol, return Java class and member info. Members are indexed
  first by name, and then by argument types to list all overloads."
  [class]
  (when-let [^Class c (try (Class/forName (str class))
                           (catch Exception _)
                           (catch LinkageError _))]
    (let [r (JavaReflector. (.getClassLoader c))] ; for dynamically loaded classes
      (misc/deep-merge (reflect-info (r/reflect c :reflector r))
                       (source-info class)
                       {:name       (-> c .getSimpleName symbol)
                        :class      (-> c .getName symbol)
                        :package    (some-> c package symbol)
                        :super      (-> c .getSuperclass typesym)
                        :interfaces (map typesym (.getInterfaces c))
                        :javadoc    (javadoc-url class)}))))

;;; ## Class Metadata Caching
;;
;; When it won't change, cache the class info. Otherwise when we analyze
;; hundreds or more classes at once (as with a naive symbol resolution call),
;; duplicated reflection and source parsing becomes a wasteful performance hit.
;;
;; To support mixed Clojure/Java projects where `.java` files are being updated
;; and recompiled, we cache such classes with last-modified property, so that we
;; know when to purge those classes from cache.

(def cache (atom {}))

(defn- immutable-source-file?
  "Return true if the source file is effectively immutable. Specifically, this
  returns true if no source file is available, or if the source file is in a
  jar/zip archive."
  [info]
  (let [path (:file info)
        src  (when path (io/resource path))]
    (or (not src)
        (re-find #"\.(jar|zip)!" (str src)))))

(defn class-info
  "For the class symbol, return (possibly cached) Java class and member info.
  Members are indexed first by name, and then by argument types to list all
  overloads."
  [class]
  (let [cached (@cache class)
        info (if cached
               (:info cached)
               (class-info* class))
        last-modified (if (immutable-source-file? info)
                        0
                        (.lastModified ^File (io/file (:path info))))
        stale (not= last-modified (:last-modified cached))
        ;; If last-modified in cache mismatches last-modified of the file,
        ;; regenerate class-info.
        info (if (and cached stale)
               (class-info* class)
               info)]
    (when (or (not cached) stale)
      (swap! cache assoc class {:info info, :last-modified last-modified}))
    info))

;;; ## Class/Member Info
;;
;; These functions filter the class info assembled above to respond to a more
;; specific query: type information for a class name, and member information for
;; a class/member combination.

(defn type-info
  "For the class or interface symbol, return Java type info. If the type has
  defined contructors, the line and column returned will be for the first of
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
  implemention. If the member is an instance member, `this` is prepended to its
  arglists."
  [class member]
  (let [c (->> (class-info class)
               (iterate (comp class-info :super))
               (take-while identity)
               (filter #(get-in % [:members member]))
               (first))]
    (when-let [m (get-in c [:members member])]
      (let [m* (first (sort-by :line (vals m)))
            static? (or (:static (:modifiers m*)) (= class member))
            +this   (comp vec (partial cons 'this))]
        (-> (dissoc m* :name :argnames)
            (assoc :class class
                   :member member
                   :file (:file c)
                   :arglists (map #((if static? identity +this)
                                    (or (:argnames %) (:argtypes %)))
                                  (vals m))
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
         (map #(member-info (-> ^Class % .getName symbol) sym))
         (filter identity)
         (distinct))))

(defn resolve-symbol
  "Given a namespace and a class or member symbol, resolve the class/member.
  Class symbols, constructors, and static calls are resolved to the class
  unambiguously. Instance members are resolved unambiguously if defined by only
  one imported class. If multiple imported classes have a member by that name, a
  map of class names to member info is returned as `:candidates`."
  [ns sym]
  {:pre [(every? symbol? [ns sym])]}
  (let [name (-> (str sym)
                 (str/replace #"^\.|\.$" "")) ; strip leading/trailing dot
        sym* (symbol name)
        [class static-member] (->> (str/split name #"/" 2)
                                   (map #(when % (symbol %))))]
    (if-let [c (resolve-class ns class)]
      (if static-member
        (member-info (:class c) static-member)      ; SomeClass/methodCall
        (type-info (:class c)))                     ; SomeClass
      (when-let [ms (seq (resolve-member ns sym*))] ; methodCall
        (if (= 1 (count ms))
          (first ms)
          {:candidates (zipmap (map :class ms) ms)})))))

(defn resolve-javadoc-path
  "Resolve a relative javadoc path to a URL and return as a map. Prefer javadoc
  resources on the classpath; then use online javadoc content for core API
  classes. If no source is available, return the relative path as is."
  [^String path]
  (or (resource/resource-full-path path)
      ;; [bug#308] `*remote-javadocs*` is outdated WRT Java
      ;; 8, so we try our own thing first.
      (when (re-find #"^(java|javax|jdk|org.omg|org.w3c.dom|org.xml.sax)/" path)
        (apply str ["https://docs.oracle.com"
                    (if (>= misc/java-api-version 11) "/en/java/javase/" "/javase/")
                    misc/java-api-version
                    "/docs/api/"
                    path]))
      ;; If that didn't work, _then_ we fallback on `*remote-javadocs*`.
      (some (let [classname (.replaceAll path "/" ".")]
              (fn [[prefix url]]
                (when (.startsWith classname prefix)
                  (str (cond-> url
                         (= 11 misc/java-api-version) (.replaceFirst "/java.base" ""))
                       path))))
            @javadoc/*remote-javadocs*)
      path))

;;; ## Initialization
;;
;; On startup, cache info for the most commonly referenced classes.
(future
  (doseq [class (->> (ns-imports 'clojure.core)
                     (map #(-> % ^Class val .getName symbol)))]
    (class-info class)))

;; TODO: Seems those were hardcoded here accidentally - we should
;; probably provide a simple API to register remote JavaDocs.
(javadoc/add-remote-javadoc "com.amazonaws." "http://docs.aws.amazon.com/AWSJavaSDK/latest/javadoc/")
(javadoc/add-remote-javadoc "org.apache.kafka." "https://kafka.apache.org/090/javadoc/index.html?")
