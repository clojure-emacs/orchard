(ns orchard.java.parser-next
  "Source and docstring info for Java classes and members. Requires JDK11+.

  Leaves `:doc` untouched.

  Adds `:doc-fragments`, `:doc-first-sentence-fragments`, and `:doc-block-tags-fragments` attributes.
  These represent sequences of 'fragments', which can be of text or html type:

  * `:doc-fragments` represents the body of the comment, including the first sentence and excluding any block tags
  * `:doc-first-sentence-fragments` represents the first sentence of the doc comment.
  * `:doc-block-tags-fragments` represent the 'param', 'returns' and 'throws' documentation.

  Clients are expected them to render the html fragments using a client-specific method,
  and then join the client-processed strings into a single string.

  Fragments of \"html\" type may have leading/trailing whitespace, which is to be ignored
  (since an HTML parser would ignored it anyway).

  Fragments of \"text\" type have significant, carefully processed leading/trailing whitespace
  such that when joining all fragments, things will look correct without having to add any extra whitespace."
  {:added "0.15.0"}
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [orchard.java.modules :as modules]
   [orchard.misc :as misc])
  (:import
   (com.sun.source.doctree BlockTagTree DocCommentTree EndElementTree
                           LinkTree LiteralTree ParamTree ReturnTree
                           StartElementTree TextTree ThrowsTree)
   (java.io StringWriter)
   (java.util.concurrent.locks ReentrantLock)
   (javax.lang.model.element Element ElementKind ExecutableElement TypeElement VariableElement)
   (javax.lang.model.type ArrayType TypeKind TypeVariable)
   (javax.tools DocumentationTool DocumentationTool$DocumentationTask ToolProvider)
   (jdk.javadoc.doclet Doclet DocletEnvironment)))

;;; ## Java Parsing
;;
;; The Java Compiler API provides in-process access to the Javadoc compiler.
;; Unlike the standard Java compiler which it extends, the Javadoc compiler
;; preserves docstrings (obviously), as well as source position and argument
;; names in its parse tree -- pieces we're after to augment reflection info.
;;
;; A few notes:
;;
;; 1. The compiler API `call` method is side-effect oriented; it returns only a
;;    boolean indicating success. To use the result parse tree, we store this in
;;    an atom.
;;
;; 2. The `result` atom must be scoped at the namespace level because a Doclet
;;    is specified by passing a class name rather than an instance; hence, we
;;    can't close over a local varaible in reify: `result` must be in scope when
;;    the methods of a *new* instance of the Doclet class are called.
;;
;; 3. To compile an individual source that is defined as part of a module, the
;;    compiler must be told to "patch" the module, and the source
;;    `JavaFileobject`'s location must match the argument to the
;;    "--patch-module" option.
;;
;;    It's not clear how to make the "--patch-module" option work with a source
;;    loaded from memory or a jar file; its syntax seems file system oriented.
;;    Moreover, if the `StandardJavaFileManager` resolves a file, the
;;    "--patch-module" option is matched, but if the exact same file is passed
;;    as a proxy-ed `Simplejavafileobject` with an identical URI, the
;;    compiler's internal `Enter` class doesn't see this as matcing the
;;    "--patch-module" option. To accommodate this, the jar file entry is
;;    written to a temp file and passed to the compiler from disk. Design-wise,
;;    this is admittedly imperfect, but the performance cost is low and it works.

;; This atom must be in top-level, not a local in `parse-java`, otherwise the
;; reify will capture it as a closure and thus will no longer have 0-arg
;; constructor, and the latter is required.
(def ^:private result (atom nil))

(defn- parse-java
  "Load and parse the resource path, returning a `DocletEnvironment` object."
  [path module]
  (when-let [res (io/resource path)]
    (let [tmpdir   (System/getProperty "java.io.tmpdir")
          tmpfile  (io/file tmpdir (.getName (io/file path)))
          ^DocumentationTool compiler (ToolProvider/getSystemDocumentationTool)
          sources  (-> (.getStandardFileManager compiler nil nil nil)
                       (.getJavaFileObjectsFromFiles [tmpfile]))
          doclet   (class (reify Doclet
                            (init [_this _ _]
                              (reset! result nil))

                            (run [_this root]
                              (reset! result root)
                              true)

                            (getSupportedOptions [_this]
                              #{})))
          out      (StringWriter.) ; discard compiler messages
          opts     (apply conj ["--show-members" "private"
                                "--show-types" "private"
                                "--show-packages" "all"
                                "--show-module-contents" "all"
                                "-quiet"]
                          (when module
                            ["--patch-module" (str module "=" tmpdir)]))
          slurped (slurp res)
          _ (spit tmpfile slurped)
          ^DocumentationTool$DocumentationTask task (.getTask compiler out nil nil doclet opts sources)]
      (try
        (if (false? (.call task))
          (throw (ex-info "Failed to parse Java source code"
                          {:path path
                           :module module
                           :out (str out)}))
          @result)
        (finally
          (.delete tmpfile))))))

;;; ## Java Parse Tree Traversal
;;
;; From the parse tree returned by the compiler, create a nested map structure
;; as produced by `orchard.java/reflect-info`: class members
;; are indexed first by name, then argument types.

(defn- typesym
  "Using parse tree info, return the type's name equivalently to the `typesym`
  function in `orchard.java`."
  ([n ^DocletEnvironment env]
   (let [t (string/replace (str n) #"<.*>" "") ; drop generics
         util (.getElementUtils env)]
     (if-let [c (.getTypeElement util t)]
       (let [pkg (str (.getPackageOf util c) ".")
             cls (-> (string/replace-first t pkg "")
                     (string/replace "." "$"))]
         (symbol (str pkg cls))) ; classes
       (symbol t)))))            ; primitives

(defn- position
  "Get line and column of `Element` e using parsed source information in env"
  [e ^DocletEnvironment env]
  (let [trees (.getDocTrees env)]
    (when-let [path (.getPath trees e)]
      (let [file (.getCompilationUnit path)
            lines (.getLineMap file)
            pos (.getStartPosition (.getSourcePositions trees)
                                   file (.getLeaf path))]
        {:line (.getLineNumber lines pos)
         :column (.getColumnNumber lines pos)}))))

(defn- parse-variable-element [^VariableElement f env]
  {:name (-> f .getSimpleName str symbol)
   :type (-> f .asType (typesym env))})

(defn- source-path
  "Return the relative `.java` source path for the top-level class."
  [klass]
  (when-let [^Class cls (resolve klass)]
    (let [path (-> (.getName cls)
                   (string/replace #"\$.*" "")
                   (string/replace "." "/")
                   (str ".java"))]
      (if-let [module (-> cls .getModule .getName)]
        (str module "/" path)
        path))))

(let [interfaces [ParamTree ThrowsTree ReturnTree BlockTagTree EndElementTree
                  LinkTree LiteralTree StartElementTree TextTree]]
  (defn dispatch [node _stack _found-closing-tags-types]
    (or (some #(when (instance? % node) %) interfaces)
        ::default)))

(defmulti process-node #'dispatch
  :default ::default)

(defn node-reducer [{:keys [stack result found-closing-tags-types] :as m}
                    node]
  {:pre [(contains? m :found-closing-tags-types)]}
  (let [[new-stack m] (process-node node stack found-closing-tags-types)]
    {:stack new-stack
     :result (into result m)
     :found-closing-tags-types found-closing-tags-types}))

(def node-reducer-init {:stack []
                        :result []})

(defmethod process-node ::default [node stack _]
  [stack
   [{:type "html"
     :content (str node)}]])

(def newline-fragment
  "A newline intended to separate html fragments.
  We choose text, because inserting <p> elements could unbalance the tags,
  given that javadocs don't necessarily consistently choose self-closing tags."
  {:type "text"
   :content "\n"})

(def nbsp "&nbsp;")

(defmethod process-node ParamTree [^ParamTree node stack found-closing-tags-types]
  (let [{:keys [stack result]} (reduce node-reducer
                                       {:stack stack
                                        :result []
                                        :found-closing-tags-types found-closing-tags-types}
                                       (.getDescription node))]
    [stack
     (reduce into [] [[newline-fragment
                       {:type "html"
                        :content (format "<i>Param</i>%s<pre>%s</pre>:%s" nbsp (.getName node) nbsp)}]
                      result])]))

(defmethod process-node ReturnTree [^ReturnTree node stack found-closing-tags-types]
  (let [{:keys [stack result]} (reduce node-reducer
                                       {:stack stack
                                        :result []
                                        :found-closing-tags-types found-closing-tags-types}
                                       (.getDescription node))]
    [stack
     (reduce into [] [[newline-fragment
                       {:type "html"
                        :content (format "<i>Returns</i>:%s" nbsp)}]
                      result])]))

(defmethod process-node ThrowsTree [^ThrowsTree node stack found-closing-tags-types]
  (let [{:keys [stack result]} (reduce node-reducer
                                       {:stack stack
                                        :result []
                                        :found-closing-tags-types found-closing-tags-types}
                                       (.getDescription node))]
    [stack
     (reduce into [] [[newline-fragment
                       {:type "html"
                        :content (format "<i>Throws</i>:%s<pre>%s</pre>:%s" nbsp (.getExceptionName node) nbsp)}]
                      result])]))

(defmethod process-node BlockTagTree [_node stack _]
  ;; omit the tag - it makes the docstring larger on docstring UIs:
  [stack []])

(defmethod process-node LiteralTree [^LiteralTree node stack _]
  (let [^String tag-name (-> node .getKind .tagName)
        body (-> node .getBody .getBody)]
    [stack
     (if (-> tag-name (.equals "code"))
       [{:type "html"
         :content (format "<pre>%s</pre> " body)}]
       [{:type "text"
         :content body}])]))

(defmethod process-node StartElementTree [^StartElementTree node stack found-closing-tags-types]
  (let [v (-> node .getName str)
        self-closing? (or (.isSelfClosing node)
                          (and (#{"p" "hr" "li"} v)
                               (not (contains? found-closing-tags-types v))))]
    [(cond-> stack
       (not self-closing?)
       (conj v))
     (cond
       (and (= v "p")
            self-closing?)
       [{:type "text"
         :content "\n"}]

       (= v "a") ;; turn links into code
       [{:type "html"
         :content "<pre>"}]

       :else
       [{:type "html"
         :content (str node)}])]))

(defmethod process-node EndElementTree [^EndElementTree node stack _]
  [(cond-> stack
     (seq stack) pop)
   [(if (-> node .getName str (.equals "a"))
      {:type "html"
       :content "</pre>"}
      {:type "html"
       :content (str node)})]])

(defmethod process-node TextTree [^TextTree node stack _]
  [stack (if (empty? stack)
           [{:type "text"
             :content (str node)}]
           [{:type "html"
             :content (str node)}])])

(defmethod process-node LinkTree [^LinkTree node stack _]
  [stack
   [{:type "html"
     :content (format "<pre>%s</pre> " (-> node .getReference .getSignature))}]])

(defn coalesce [xs]
  (reduce (fn [acc {next-type :type next-content :content :as next-item}]
            (let [{prev-type :type} (peek acc)]
              (if (= prev-type next-type)
                (update-in acc
                           [(dec (count acc)) :content]
                           str
                           (if (= prev-type "text")
                             "\n\n"
                             " ")
                           next-content)
                (conj acc next-item))))
          []
          xs))

(defn remove-left-margin [s]
  (->> (string/split s #"\r?\n" -1) ;; split-lines without losing trailing newlines
       (map-indexed (fn [i s]
                      (let [first? (zero? i)
                            blank? (string/blank? s)]
                        (cond-> s
                          (and (not first?)
                               (not blank?))
                          (string/replace #"^ +" "")))))
       (string/join "\n")))

(defn cleanup-whitespace [fragments]
  (into []
        (map (fn [{:keys [content]
                   content-type :type
                   :as x}]
               (let [text? (= content-type "text")]
                 (assoc x :content (-> content
                                       (string/replace #"^  +" " ")
                                       (string/replace #"  +$" " ")
                                       (string/replace #"\s*\n+\s*\n+\s*" "\n\n")
                                       (string/replace #"\n +$" "\n")
                                       (cond-> text? remove-left-margin
                                               text? (string/replace #"^ +\." ".")
                                               text? (string/replace #"^ +," ",")))))))
        fragments))

(defn docstring
  "Get parsed docstring text of `e` using source information in env"
  [^Element e ^DocletEnvironment env]
  (let [^DocCommentTree comment-tree (some-> env
                                             .getDocTrees
                                             (.getDocCommentTree e))
        full-body-raw (some->> comment-tree .getFullBody)
        block-tags-raw (some->> comment-tree .getBlockTags)
        found-closing-tags-types (into #{}
                                       (keep #(when (instance? EndElementTree %)
                                                (str (.getName ^EndElementTree %))))
                                       (into (vec full-body-raw)
                                             (vec block-tags-raw)))
        full-body (some->> full-body-raw
                           (reduce node-reducer (assoc node-reducer-init :found-closing-tags-types found-closing-tags-types))
                           :result)
        block-tags (some->> block-tags-raw
                            (reduce node-reducer (assoc node-reducer-init :found-closing-tags-types found-closing-tags-types))
                            :result)
        first-sentence (some->> comment-tree
                                .getFirstSentence
                                (reduce node-reducer (assoc node-reducer-init
                                                            :found-closing-tags-types
                                                            found-closing-tags-types))
                                :result)]
    {:doc (some-> env .getElementUtils (.getDocComment e) string/trim)
     :doc-first-sentence-fragments (-> first-sentence coalesce cleanup-whitespace)
     :doc-fragments (-> full-body coalesce cleanup-whitespace)
     :doc-block-tags-fragments (-> block-tags coalesce cleanup-whitespace)}))

(defprotocol Parsed
  (parse-info* [o env]))

(defn parse-info
  [o env]
  (merge (parse-info* o env)
         (docstring o env)
         (position o env)))

(defn parse-executable-element [^ExecutableElement m env]
  (let [parameters (.getParameters m)
        type->sym #(-> ^VariableElement % .asType (typesym env))
        upper-bound #(if (instance? TypeVariable %)
                       (.getUpperBound ^TypeVariable %)
                       %)]
    {:name (if (= (.getKind m) ElementKind/CONSTRUCTOR)
             (-> m .getEnclosingElement (typesym env)) ; class name
             (-> m .getSimpleName str symbol))         ; method name
     :type (-> m .getReturnType (typesym env))
     :argtypes (mapv type->sym parameters)
     :non-generic-argtypes
     (mapv (fn [^Element element]
             (let [type (.asType element)
                   kind (.getKind type)
                   best (some-> (cond (= kind TypeKind/ARRAY)
                                      (or (some-> (upper-bound (.getComponentType ^ArrayType type))
                                                  (str "[]"))
                                          type)

                                      (= kind TypeKind/TYPEVAR)
                                      (upper-bound type))
                                str
                                misc/remove-type-param
                                symbol)]
               (some-> (or best
                           (type->sym element))
                       str
                       (string/replace "$" ".")
                       symbol)))
           parameters)
     :argnames (mapv #(-> ^VariableElement % .getSimpleName str symbol) (.getParameters m))}))

(extend-protocol Parsed
  TypeElement ; => class, interface, enum
  (parse-info* [c env]
    {:class   (typesym c env)
     :members (->> (.getEnclosedElements c)
                   (filterv #(#{ElementKind/CONSTRUCTOR
                                ElementKind/METHOD
                                ElementKind/FIELD
                                ElementKind/ENUM_CONSTANT}
                              (.getKind ^Element %)))
                   (mapv #(parse-info % env))
                   ;; Index by name, argtypes. Args for fields are nil.
                   (group-by :name)
                   (reduce (fn [ret [n ms]]
                             (assoc ret n (zipmap (mapv :non-generic-argtypes ms) ms)))
                           {}))})

  ExecutableElement ;; => method, constructor
  (parse-info* [o env]
    (parse-executable-element o env))

  VariableElement ;; => field, enum constant
  (parse-info* [o env]
    (parse-variable-element o env)))

(def ^:private lock (ReentrantLock.))

(defn source-info
  "If the source for the Java class is available on the classpath, parse it
  and return info to supplement reflection. Specifically, this includes source
  file and position, docstring, and argument name info. Info returned has the
  same structure as that of `orchard.java/reflect-info`."
  [klass]
  {:pre [(symbol? klass)]}
  (misc/with-lock lock ;; the jdk.javadoc.doclet classes aren't meant for concurrent modification/access.
    (when-let [path (source-path klass)]
      (when-let [^DocletEnvironment root (parse-java path (modules/module-name klass))]
        (try
          (let [path-resource (io/resource path)]
            (assoc (some #(when (#{ElementKind/CLASS
                                   ElementKind/INTERFACE
                                   ElementKind/ENUM}
                                 (.getKind ^Element %))
                            (let [info (parse-info % root)]
                              (when (= (:class info) klass)
                                info)))
                         (.getIncludedElements root))
                   ;; relative path on the classpath
                   :file path
                   ;; Legacy key. Please do not remove - we don't do breaking changes!
                   :path (.getPath path-resource)
                   ;; Full URL, e.g. file:.. or jar:...
                   :resource-url path-resource))
          (finally (.close (.getJavaFileManager root))))))))
