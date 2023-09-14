(ns orchard.java.parser-next
  "Source and docstring info for Java classes and members.

  Leaves `:doc` untouched.

  Adds `:doc-fragments` and `:doc-first-sentence-fragments` attributes.
  Both represent sequences of 'fragments', which can be of text or html type.
  Clients are expected them to render the html fragments using a client-specific method,
  and then join the client-processed by a newline."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [orchard.java.parser-utils :refer [module-name parse-executable-element parse-java parse-variable-element position source-path typesym]])
  (:import
   (com.sun.source.doctree DocCommentTree)
   (javax.lang.model.element Element ElementKind ExecutableElement TypeElement VariableElement)
   (jdk.javadoc.doclet DocletEnvironment)))

(defn dispatch [node _stack]
  (cond
    (-> node class (= com.sun.tools.javac.tree.DCTree$DCParam))
    ::param

    (-> node class (= com.sun.tools.javac.tree.DCTree$DCThrows))
    ::throws

    (-> node class (= com.sun.tools.javac.tree.DCTree$DCReturn))
    ::return

    (->> node class ancestors (some #{com.sun.tools.javac.tree.DCTree$DCBlockTag}))
    ::block-tag

    :else (class node)))

(defmulti process-node #'dispatch
  :default ::default)

(defn node-reducer [{:keys [stack result]} node]
  (let [[new-stack m] (process-node node stack)]
    {:stack new-stack
     :result (into result m)}))

(def node-reducer-init {:stack []
                        :result []})

(defmethod process-node ::default [node stack]
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

(defmethod process-node ::param [^com.sun.tools.javac.tree.DCTree$DCParam node stack]
  (let [{:keys [stack result]} (reduce node-reducer
                                       {:stack stack
                                        :result []}
                                       (.getDescription node))]
    [stack
     (reduce into [] [[newline-fragment
                       {:type "html"
                        :content (format "<i>Param</i>%s<>%s</pre>:%s" nbsp (.getName node) nbsp)}]
                      result])]))

(defmethod process-node ::return [^com.sun.tools.javac.tree.DCTree$DCReturn node stack]
  (let [{:keys [stack result]} (reduce node-reducer
                                       {:stack stack
                                        :result []}
                                       (.getDescription node))]
    [stack
     (reduce into [] [[newline-fragment
                       {:type "html"
                        :content (format "<i>Returns</i>:%s" nbsp)}]
                      result])]))

(defmethod process-node ::throws [^com.sun.tools.javac.tree.DCTree$DCThrows node stack]
  (let [{:keys [stack result]} (reduce node-reducer
                                       {:stack stack
                                        :result []}
                                       (.getDescription node))]
    [stack
     (reduce into [] [[newline-fragment
                       {:type "html"
                        :content (format "<i>Throws</i>:%s<pre>%s</pre>:%s" nbsp (.getExceptionName node) nbsp)}]
                      result])]))

(defmethod process-node ::block-tag [^com.sun.tools.javac.tree.DCTree$DCBlockTag node stack]
  (let [tag-name (.getTagName node)]
    (if (.equals tag-name "author")
      ;; omit the tag - it makes the docstring larger on docstring UIs:
      [stack []]
      (let [tag-name (.getTagName node)
            s (string/replace (str node) #"^@" "")
            [_ b] (string/split s (re-pattern tag-name))
            content (when b
                      (let [bt (string/trim b)]
                        (case tag-name
                          ("implNote" "jls") (format "<i>%s</i>: %s" tag-name bt)
                          (format "<i>%s</i>: <pre>%s</pre>" tag-name bt))))]
        [stack
         (if content
           [newline-fragment
            {:type "html"
             :content content}]
           [])]))))

;; com.sun.tools.javac.tree.DCTree$DCUnknownInlineTag
"{@jls 3.10.5}"

(defmethod process-node com.sun.tools.javac.tree.DCTree$DCLiteral [^com.sun.tools.javac.tree.DCTree$DCLiteral node stack]
  [stack
   ;; if code:
   [{:type "html"
     :content (format "<pre>%s</pre> " (-> node .getBody .getBody))}]
   ;; XXX
   ;; else, tag as <b>, content as text
   ])

(defmethod process-node com.sun.tools.javac.tree.DCTree$DCStartElement [^com.sun.tools.javac.tree.DCTree$DCStartElement node
                                                                        stack]
  (let [v (-> node .getName str)
        self-closing? (or (.isSelfClosing node)
                          ;; XXX only do this if no neighbor closing tag shares the same (-> node .getName str):
                          (#{"p" "hr" "li"} v))]
    [(cond-> stack
       (not self-closing?)
       (conj v))
     (if (and (= v "p")
              self-closing?)
       [{:type "text"
         :content "\n"}]
       [{:type "html"
         :content (str node)}])]))

(defmethod process-node com.sun.tools.javac.tree.DCTree$DCEndElement [node stack]
  [(cond-> stack
     (seq stack) pop)
   [{:type "html"
     :content (str node)}]])

(defmethod process-node com.sun.tools.javac.tree.DCTree$DCText [node stack]
  [stack (if (empty? stack)
           [{:type "text"
             :content (str node)}]
           [{:type "html"
             :content (str node)}])])

(defmethod process-node com.sun.tools.javac.tree.DCTree$DCLink [^com.sun.tools.javac.tree.DCTree$DCLink node stack]
  [stack
   [{:type "html"
     :content (format "<pre>%s</pre> " (-> node .getReference .getSignature))}]])

(defn coalesce [xs]
  (reduce (fn [acc {next-type :type next-content :content :as next-item}]
            (let [{prev-type :type} (peek acc)]
              (if (= prev-type next-type)
                (update-in acc [(dec (count acc)) :content] str " " next-content)
                (conj acc next-item))))
          []
          (into []
                (remove (comp string/blank? :content))
                xs)))

(defn cleanup-whitespace [fragments]
  (into []
        (map (fn [{:keys [content] :as x}]
               (assoc x :content (-> content
                                     string/trim
                                     (string/replace #"\s*\n+\s*\n+\s*" "\n\n")))))
        fragments))

(defn docstring
  "Get parsed docstring text of `e` using source information in env"
  [^Element e ^DocletEnvironment env]
  (let [^DocCommentTree comment-tree (some-> env
                                             .getDocTrees
                                             (.getDocCommentTree e))
        full-body (some->> comment-tree
                           .getFullBody
                           (reduce node-reducer node-reducer-init)
                           :result)
        block-tags (some->> comment-tree
                            .getBlockTags
                            (reduce node-reducer node-reducer-init)
                            :result)
        first-sentence (some->> comment-tree
                                .getFirstSentence
                                (reduce node-reducer node-reducer-init)
                                :result)]
    {:doc (some-> env .getElementUtils (.getDocComment e) string/trim)
     :doc-first-sentence-fragments (-> first-sentence coalesce cleanup-whitespace)
     :doc-fragments (-> (into full-body block-tags)
                        coalesce
                        cleanup-whitespace)}))

(defprotocol Parsed
  (parse-info* [o env]))

(defn parse-info
  [o env]
  (merge (parse-info* o env)
         (docstring o env)
         (position o env)))

(extend-protocol Parsed
  TypeElement ; => class, interface, enum
  (parse-info* [c env]
    {:class   (typesym c env)
     :members (->> (.getEnclosedElements c)
                   (filter #(#{ElementKind/CONSTRUCTOR
                               ElementKind/METHOD
                               ElementKind/FIELD
                               ElementKind/ENUM_CONSTANT}
                             (.getKind ^Element %)))
                   (map #(parse-info % env))
                   ;; Index by name, argtypes. Args for fields are nil.
                   (group-by :name)
                   (reduce (fn [ret [n ms]]
                             (assoc ret n (zipmap (map :argtypes ms) ms)))
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
