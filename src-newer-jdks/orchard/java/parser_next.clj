(ns orchard.java.parser-next
  "Source and docstring info for Java classes and members.

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
   [orchard.java.parser-utils :refer [module-name parse-java parse-variable-element position source-path typesym]])
  (:import
   (com.sun.source.doctree DocCommentTree)
   (javax.lang.model.element Element ElementKind ExecutableElement TypeElement VariableElement)
   (jdk.javadoc.doclet DocletEnvironment)))

(defn dispatch [node _stack _found-closing-tags-types]
  (cond
    (instance? com.sun.tools.javac.tree.DCTree$DCParam node)
    ::param

    (instance? com.sun.tools.javac.tree.DCTree$DCThrows node)
    ::throws

    (instance? com.sun.tools.javac.tree.DCTree$DCReturn node)
    ::return

    (->> node class ancestors (some #{com.sun.tools.javac.tree.DCTree$DCBlockTag}))
    ::discard

    :else (class node)))

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

(defmethod process-node ::param [^com.sun.tools.javac.tree.DCTree$DCParam node stack found-closing-tags-types]
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

(defmethod process-node ::return [^com.sun.tools.javac.tree.DCTree$DCReturn node stack found-closing-tags-types]
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

(defmethod process-node ::throws [^com.sun.tools.javac.tree.DCTree$DCThrows node stack found-closing-tags-types]
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

(defmethod process-node ::discard [_node stack _]
  ;; omit the tag - it makes the docstring larger on docstring UIs:
  [stack []])

(defmethod process-node com.sun.tools.javac.tree.DCTree$DCLiteral [^com.sun.tools.javac.tree.DCTree$DCLiteral node stack _]
  (let [^String tag-name (-> node .getKind .tagName)
        body (-> node .getBody .getBody)]
    [stack
     (if (-> tag-name (.equals "code"))
       [{:type "html"
         :content (format "<pre>%s</pre> " body)}]
       [{:type "text"
         :content body}])]))

(defmethod process-node com.sun.tools.javac.tree.DCTree$DCStartElement [^com.sun.tools.javac.tree.DCTree$DCStartElement node
                                                                        stack
                                                                        found-closing-tags-types]
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

(defmethod process-node com.sun.tools.javac.tree.DCTree$DCEndElement [^com.sun.tools.javac.tree.DCTree$DCEndElement node
                                                                      stack
                                                                      _]
  [(cond-> stack
     (seq stack) pop)
   [(if (-> node .getName str (.equals "a"))
      {:type "html"
       :content "</pre>"}
      {:type "html"
       :content (str node)})]])

(defmethod process-node com.sun.tools.javac.tree.DCTree$DCText [node stack _]
  [stack (if (empty? stack)
           [{:type "text"
             :content (str node)}]
           [{:type "html"
             :content (str node)}])])

(defmethod process-node com.sun.tools.javac.tree.DCTree$DCLink [^com.sun.tools.javac.tree.DCTree$DCLink node stack _]
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
                                       (comp (filter (comp #{com.sun.tools.javac.tree.DCTree$DCEndElement} class))
                                             (map (fn [^com.sun.tools.javac.tree.DCTree$DCEndElement e]
                                                    (-> e .getName str))))
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

(defn remove-type-param [s]
  (-> s
      (string/replace #"<.*" "")
      (string/replace #"\[.*" "")))

(defn parse-executable-element [^ExecutableElement m env]
  (let [parameters (.getParameters m)
        type->sym #(-> ^VariableElement % .asType (typesym env))]
    {:name (if (= (.getKind m) ElementKind/CONSTRUCTOR)
             (-> m .getEnclosingElement (typesym env)) ; class name
             (-> m .getSimpleName str symbol))         ; method name
     :type (-> m .getReturnType (typesym env))
     :argtypes (mapv type->sym parameters)
     :non-generic-argtypes (mapv (fn [^com.sun.tools.javac.code.Symbol element]
                                   (let [ast (-> element .asType)
                                         ast-str (-> ast .getKind str)
                                         best (when (or (.equals ast-str "TYPEVAR")
                                                        (.equals ast-str "ARRAY"))
                                                (some-> (or (when (instance? com.sun.tools.javac.code.Type$ArrayType
                                                                             ast)
                                                              (-> ^com.sun.tools.javac.code.Type$ArrayType ast
                                                                  .getComponentType
                                                                  .getUpperBound))
                                                            (some-> ast .getUpperBound)
                                                            (some-> ast .getOriginalType))
                                                        str
                                                        remove-type-param
                                                        symbol))]
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
                   (filter #(#{#_ElementKind/CONSTRUCTOR ;; will be enabled when it's also properly implemented at reflector level
                               ElementKind/METHOD
                               ElementKind/FIELD
                               ElementKind/ENUM_CONSTANT}
                             (.getKind ^Element %)))
                   (map #(parse-info % env))
                   ;; Index by name, argtypes. Args for fields are nil.
                   (group-by :name)
                   (reduce (fn [ret [n ms]]
                             (assoc ret n (zipmap (map :non-generic-argtypes ms) ms)))
                           {}))})

  ExecutableElement ;; => method, constructor
  (parse-info* [o env]
    (parse-executable-element o env))

  VariableElement ;; => field, enum constant
  (parse-info* [o env]
    (parse-variable-element o env)))

(def lock (Object.))

(defn source-info
  "If the source for the Java class is available on the classpath, parse it
  and return info to supplement reflection. Specifically, this includes source
  file and position, docstring, and argument name info. Info returned has the
  same structure as that of `orchard.java/reflect-info`."
  [klass]
  {:pre [(symbol? klass)]}
  (locking lock ;; the jdk.javadoc.doclet classes aren't meant for concurrent modification/access.
    (try
      (when-let [path (source-path klass)]
        (when-let [^DocletEnvironment root (parse-java path (module-name klass))]
          (try
            (let [path-resource (io/resource path)]
              (assoc (->> (.getIncludedElements root)
                          (filter #(#{ElementKind/CLASS
                                      ElementKind/INTERFACE
                                      ElementKind/ENUM}
                                    (.getKind ^Element %)))
                          (map #(parse-info % root))
                          (filter #(= klass (:class %)))
                          (first))
                     ;; relative path on the classpath
                     :file path
                     ;; Legacy key. Please do not remove - we don't do breaking changes!
                     :path (.getPath path-resource)
                     ;; Full URL, e.g. file:.. or jar:...
                     :resource-url path-resource))
            (finally (.close (.getJavaFileManager root))))))
      (catch Throwable e
        (when (= "true" (System/getProperty "orchard.internal.test-suite-running"))
          (throw e))))))
