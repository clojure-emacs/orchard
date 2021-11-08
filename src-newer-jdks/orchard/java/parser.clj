(ns orchard.java.parser
  "Source and docstring info for Java classes and members"
  {:author "Jeff Valk"}
  (:refer-clojure :exclude [resolve])
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   (java.io StringReader StringWriter)
   (javax.lang.model.element Element ElementKind ExecutableElement
                             TypeElement VariableElement)
   (javax.swing.text.html HTML$Tag HTMLEditorKit$ParserCallback)
   (javax.swing.text.html.parser ParserDelegator)
   (javax.tools ToolProvider)
   (jdk.javadoc.doclet Doclet DocletEnvironment)))

;;; ## JDK Compatibility
;;
;; This namespace requires JDK9+.

;;; ## Java Source Analysis
;;
;; Any metadata not available via reflection can be had from the source code; we
;; just need to parse it. In the case of docstrings, we actually need to parse
;; it twice -- first from Java source, and then from Javadoc HTML.

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
                            (init [this _ _] (reset! result nil))
                            (run [this root] (reset! result root) true)
                            (getSupportedOptions [this] #{})))
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
;; Unlike source metadata (line, position, etc) that's available directly from
;; the compiler parse tree, docstrings are "some assembly required." Javadoc
;; comments use both `@tags` and HTML <tags> for semantics and formatting. The
;; latter could be passed through intact if our presentation layer could read
;; it, but we want a pure text representation, so we'll parse html to markdown.
;; This way it can either be rendered or displayed as text.

;; Use GFM extensions for multiline code blocks and tables.
(def markdown
  "Syntax map from html tag to a tuple of tag type key, start, and end chars"
  (let [char-map {:p     ["\n\n"]     :code  ["`" "`"]
                  :br    ["\n"]       :code* ["\n\n```\n" "```\n\n"]
                  :em    ["*" "*"]    :table ["\n|--" "\n|--"]
                  :str   ["**" "**"]  :thead ["" "|--\n"]
                  :list  ["\n"]       :tr    ["\n" "|"]
                  :li    ["- "]       :td    ["|"]
                  :dd    [": "]       :th    ["|"]}
        tags     {HTML$Tag/P  :p           HTML$Tag/TT    :code
                  HTML$Tag/BR :br          HTML$Tag/CODE  :code
                  HTML$Tag/I  :em          HTML$Tag/VAR   :code
                  HTML$Tag/EM :em          HTML$Tag/KBD   :code
                  HTML$Tag/B  :str         HTML$Tag/PRE   :code*
                  HTML$Tag/STRONG :str     HTML$Tag/BLOCKQUOTE :code*
                  HTML$Tag/UL :list        HTML$Tag/TABLE :table
                  HTML$Tag/OL :list        HTML$Tag/TR    :tr
                  HTML$Tag/DL :list        HTML$Tag/TD    :td
                  HTML$Tag/LI :li          HTML$Tag/TH    :th
                  HTML$Tag/DT :li
                  HTML$Tag/DD :dd}]
    (-> (reduce (fn [tags [tag k]]
                  (assoc tags tag (cons k (char-map k))))
                {} tags)
        (with-meta char-map))))

;; The HTML parser and DTD classes are in the `javax.swing` package, and have
;; internal references to the `sun.awt.AppContext` class. On Mac OS X, any use
;; of this class causes a stray GUI window to pop up. Setting the system
;; property below prevents this. We only set the property if it
;; hasn't already been explicitly set.
(when (nil? (System/getProperty "apple.awt.UIElement"))
  (System/setProperty "apple.awt.UIElement" "true"))

;; We parse html and emit text in a single pass -- there's no need to build a
;; tree. The syntax map defines most of the output format, but a few stateful
;; rules are applied:
;;
;; 1. List items are indented to their nested depth.
;; 2. Nested elements with the same tag type key are coalesced (`<pre>` inside
;;    of `<blockquote>` is common, for instance).
;; 3. A border row is inserted between `<th>` and `<td>` table rows. Since
;;    `<thead>` and `<tbody>` are optional, we look for the th/td transition.
(defn parse-html
  "Parse html to markdown text."
  [html]
  (let [sb (StringBuilder.)
        sr (StringReader. html)
        parser (ParserDelegator.)
        stack (atom nil)
        flags (atom #{})
        handler (proxy [HTMLEditorKit$ParserCallback] []
                  (handleText [^chars chars _]
                    (.append sb (String. chars)))

                  (handleStartTag [tag _ _]
                    (let [[k start] (markdown tag)]
                      (when (and k (not= k (peek @stack)))
                        (swap! stack conj k)

                        ;; Indent list items at the current depth.
                        (when (#{:li} k)
                          (let [depth (count (filter #{:list} @stack))]
                            (.append sb "\n")
                            (dotimes [_ (dec depth)]
                              (.append sb "  "))))

                        ;; Keep th/td state; emit border between th and td rows.
                        (when (#{:th} k) (swap! flags conj :th))
                        (when (and (#{:td} k) (@flags :th))
                          (.append sb (-> markdown meta :thead last)))

                        (when start (.append sb start)))))

                  (handleEndTag [tag _]
                    (let [[k _ end] (markdown tag)]
                      (when (and k (= k (peek @stack)))
                        (swap! stack pop)
                        (when (#{:table :td} k) (swap! flags disj :th))
                        (when end (.append sb end))))))]

    (.parse parser sr handler false)
    (-> (str sb)
        (str/replace #"\n{3,}" "\n\n") ; normalize whitespace
        (str/replace #" +```" "```"))))

(defn docstring
  "Get parsed docstring text of `Element` e using source information in env"
  [e ^DocletEnvironment env]
  ;;
  ;; NOTE This returns tags (e.g. @link) literally. To parse and resolve these,
  ;; we probably need to use: `(-> env .getDocTrees (.getDocCommentTree e))`.
  ;; That's an enhancement for another day.
  {:doc (some-> env .getElementUtils (.getDocComment e) parse-html)})

;;; ## Java Parse Tree Traversal
;;
;; From the parse tree returned by the compiler, create a nested map structure
;; as produced by `orchard.java/reflect-info`: class members
;; are indexed first by name, then argument types.

(defn typesym
  "Using parse tree info, return the type's name equivalently to the `typesym`
  function in `orchard.java`."
  ([n ^DocletEnvironment env]
   (let [t (str/replace (str n) #"<.*>" "") ; drop generics
         util (.getElementUtils env)]
     (if-let [c (.getTypeElement util t)]
       (let [pkg (str (.getPackageOf util c) ".")
             cls (-> (str/replace-first t pkg "")
                     (str/replace "." "$"))]
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
                   (str/replace #"\$.*" "")
                   (str/replace "." "/")
                   (str ".java"))]
      (if-let [module (-> cls .getModule .getName)]
        (str module "/" path)
        path))))

(defn source-info
  "If the source for the Java class is available on the classpath, parse it
  and return info to supplement reflection. Specifically, this includes source
  file and position, docstring, and argument name info. Info returned has the
  same structure as that of `orchard.java/reflect-info`."
  [klass]
  {:pre [(symbol? klass)]}
  (try
    (when-let [path (source-path klass)]
      (when-let [^DocletEnvironment root (parse-java path (module-name klass))]
        (try
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
                 :path (-> path io/resource .getPath)
                 ;; Full URL, e.g. file:.. or jar:...
                 :resource-url (io/resource path))
          (finally (.close (.getJavaFileManager root))))))
    (catch Throwable _)))
