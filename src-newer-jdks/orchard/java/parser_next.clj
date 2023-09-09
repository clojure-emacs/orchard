(ns orchard.java.parser-next
  "Source and docstring info for Java classes and members"
  (:refer-clojure :exclude [resolve])
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string])
  (:import
   (java.io StringReader StringWriter)
   (javax.lang.model.element Element ElementKind ExecutableElement TypeElement VariableElement)
   (javax.swing.text.html HTML$Tag HTMLEditorKit$ParserCallback)
   (javax.swing.text.html.parser ParserDelegator)
   (javax.tools ToolProvider)
   (jdk.javadoc.doclet Doclet DocletEnvironment)
   com.sun.source.doctree.DocCommentTree))

;; XXX extract utils
;; XXX remove third+ newlines/ws

;; XXX global
(def result (atom nil))

(defn parse-java
  "Load and parse the resource path, returning a `DocletEnvironment` object."
  [path module]
  (when-let [res (io/resource path)]
    (let [tmpdir   (System/getProperty "java.io.tmpdir")
          tmpfile  (io/file tmpdir (.getName (io/file path)))
          compiler (ToolProvider/getSystemDocumentationTool)
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
                            ["--patch-module" (str module "=" tmpdir)]))]
      (spit tmpfile (slurp res))
      (.call (.getTask compiler out nil nil doclet opts sources))
      (.delete tmpfile)
      @result)))

;;; ## Docstring Parsing
;;

(def the-block-class ;; XXX try/catch
  com.sun.tools.javac.tree.DCTree$DCBlockTag)

;; The HTML parser and DTD classes are in the `javax.swing` package, and have
;; internal references to the `sun.awt.AppContext` class. On Mac OS X, any use
;; of this class causes a stray GUI window to pop up. Setting the system
;; property below prevents this. We only set the property if it
;; hasn't already been explicitly set.
(when (nil? (System/getProperty "apple.awt.UIElement"))
  (System/setProperty "apple.awt.UIElement" "true"))

(defn dispatch [node stack]
  (cond
    (-> node class .getName (= "com.sun.tools.javac.tree.DCTree$DCParam"))
    ::param

    (-> node class .getName (= "com.sun.tools.javac.tree.DCTree$DCThrows"))
    ::throws

    (-> node class .getName (= "com.sun.tools.javac.tree.DCTree$DCReturn"))
    ::return

    (->> node class ancestors (filter class?) (map (fn [c]
                                                     (.getName c)))
         (some #{"com.sun.tools.javac.tree.DCTree$DCBlockTag"}))
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

(def zz (atom []))

(defmethod process-node ::default [node stack]
  (swap! zz conj node)
  [stack
   [{:type "html"
     :content (str node)}]])

(comment
  (-> @params first .getDescription))

(def KO (atom nil))

(def newline-fragment
  "A newline intended to separate html fragments.
  We choose text, because inserting <p> elements could unbalance the tags,
  given that javadocs don't necessarily consistently choose self-closing tags."
  {:type "text"
   :content "\n"})

(def nbsp "&nbsp;")

(defmethod process-node ::param [^com.sun.tools.javac.tree.DCTree$DCParam node stack]
  ;; (swap! paramsR conj)
  (let [{:keys [stack result] :as x} (reduce node-reducer
                                             {:stack stack
                                              :result []}
                                             (.getDescription node))]
    (when (seq result)
      (reset! KO x))
    [stack
     (reduce into [[newline-fragment
                    {:type "html"
                     :content (format "<i>Param</i>%s<>%s</pre>:%s" nbsp (.getName node) nbsp)}]
                   result])]))

(def params (atom []))

(defmethod process-node ::return [^com.sun.tools.javac.tree.DCTree$DCReturn node stack]
  (swap! params conj node)
  (let [{:keys [stack result] :as x} (reduce node-reducer
                                             {:stack stack
                                              :result []}
                                             (.getDescription node))]
    (when (seq result)
      (reset! KO x))
    [stack
     (reduce into [[newline-fragment
                    {:type "html"
                     :content (format "<i>Returns</i>:%s" nbsp)}]
                   result])]))

(def throws (atom []))

(def throwsR (atom []))

(defmethod process-node ::throws [ ;; ^com.sun.tools.javac.tree.DCTree$DCBlockTag
                                  node stack]
  (swap! throws conj node)
  (let [{:keys [stack result]} (reduce node-reducer
                                       {:stack stack
                                        :result []}
                                       (.getDescription node))]
    [stack
     (reduce into [[newline-fragment
                    {:type "html"
                     :content (format "<i>Throws</i>:%s<pre>%s</pre>:%s" nbsp (.getExceptionName node) nbsp)}]
                   result])]))

(def blocks (atom []))
(defmethod process-node ::block-tag [ ;; ^com.sun.tools.javac.tree.DCTree$DCBlockTag
                                     node stack]
  (swap! blocks conj node)
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

(def aa (atom []))
(defmethod process-node com.sun.tools.javac.tree.DCTree$DCLiteral [node stack]
  (swap! aa conj node)
  [stack
   ;; if code:
   [{:type "html"
     :content (format "<pre>%s</pre> "(-> node .getBody .getBody))}]
   ;; XXX
   ;; else, tag as <b>, content as text
   ])

(def bb (atom []))
(defmethod process-node com.sun.tools.javac.tree.DCTree$DCStartElement [node stack]
  (swap! bb conj node)
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

(def cc (atom []))
(defmethod process-node com.sun.tools.javac.tree.DCTree$DCEndElement [node stack]
  (swap! cc conj node)
  [(cond-> stack
     (seq stack) pop)
   [{:type "html"
     :content (str node)}]])

(def dd (atom []))
(defmethod process-node com.sun.tools.javac.tree.DCTree$DCText [node stack]
  (swap! dd conj node)
  [stack (if (empty? stack)
           [{:type "text"
             :content (str node)}]
           [{:type "html"
             :content (str node)
             :stack-here stack}])])

(def ee (atom []))
(defmethod process-node com.sun.tools.javac.tree.DCTree$DCLink [node stack]
  (swap! ee conj node)
  [stack
   [{:type "html"
     :content (format "<pre>%s</pre> "(-> node .getReference .getSignature))}]])

(def C (atom nil))

(defn coalesce [xs]
  (reduce (fn [acc {next-type :type next-content :content :as next-item}]
            (let [{prev-type :type prev-content :content} (peek acc)]
              (if (= prev-type next-type)
                (update-in acc [(dec (count acc)) :content] str " " next-content)
                (conj acc next-item))))
          []
          (into []
                (remove (comp empty? :content))
                xs)))


(def mkmk (atom []))

(comment
  (->> @mkmk
       (filter (every-pred (comp seq :full-body) (comp seq :block-tags)))
       last
       vals
       (apply into)
       (coalesce)))

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
    (swap! mkmk conj {:full-body full-body :block-tags block-tags})
    {:doc (some-> env .getElementUtils (.getDocComment e))
     :doc-first-sentence-fragments (coalesce first-sentence)
     :doc-fragments (coalesce (into full-body block-tags))}))

;;; ## Java Parse Tree Traversal
;;
;; From the parse tree returned by the compiler, create a nested map structure
;; as produced by `orchard.java/reflect-info`: class members
;; are indexed first by name, then argument types.

(defn typesym
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

(defn position
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

(defprotocol Parsed
  (parse-info* [o env]))

(defn parse-info
  [o env]
  (merge (parse-info* o env)
         (docstring o env)
         (position o env)))

(extend-protocol Parsed
  ExecutableElement ; => method, constructor
  (parse-info* [m env]
    {:name (if (= (.getKind m) ElementKind/CONSTRUCTOR)
             (-> m .getEnclosingElement (typesym env)) ; class name
             (-> m .getSimpleName str symbol))         ; method name
     :type (-> m .getReturnType (typesym env))
     :argtypes (mapv #(-> ^VariableElement % .asType (typesym env)) (.getParameters m))
     :argnames (mapv #(-> ^VariableElement % .getSimpleName str symbol) (.getParameters m))})

  VariableElement ; => field, enum constant
  (parse-info* [f env]
    {:name (-> f .getSimpleName str symbol)
     :type (-> f .asType (typesym env))})

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
                           {}))}))

(defn- resolve
  "Workaround for CLJ-1403, fixed in Clojure 1.10. Once 1.9 support is
  discontinued, this function may simply be removed."
  [sym]
  (try (clojure.core/resolve sym)
       (catch Exception _)))

(defn module-name
  "Return the module name, or nil if modular"
  [klass]
  (some-> klass ^Class resolve .getModule .getName))

(defn source-path
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
      (catch Throwable _ (throw _)))))

(comment

  (do
    (source-info `String)
    Thread/sleep
    (-> (source-info `Thread)
        (get-in [:members 'sleep ['long] :doc-fragments])
        (clojure.pprint/pprint ))

    ;; XXX
    Thread/sleep ;; has params, not rendered
    Thread/onSpinWait ;; has @code inside <pre>, should be unwrapped
    nil))

;; a good unit test: every present doc for string and thread has doc-fragments counterpart
