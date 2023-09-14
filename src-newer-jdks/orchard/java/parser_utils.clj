(ns orchard.java.parser-utils
  (:refer-clojure :exclude [resolve])
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string])
  (:import
   (java.io StringWriter)
   (javax.lang.model.element ElementKind ExecutableElement VariableElement)
   (javax.tools ToolProvider)
   (jdk.javadoc.doclet Doclet DocletEnvironment)))

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

(defn parse-executable-element [^ExecutableElement m env]
  {:name (if (= (.getKind m) ElementKind/CONSTRUCTOR)
           (-> m .getEnclosingElement (typesym env)) ; class name
           (-> m .getSimpleName str symbol)) ; method name
   :type (-> m .getReturnType (typesym env))
   :argtypes (mapv #(-> ^VariableElement % .asType (typesym env)) (.getParameters m))
   :argnames (mapv #(-> ^VariableElement % .getSimpleName str symbol) (.getParameters m))})

(defn parse-variable-element [^VariableElement f env]
  {:name (-> f .getSimpleName str symbol)
   :type (-> f .asType (typesym env))})

(defn- resolve
  "Workaround for CLJ-1403, fixed in Clojure 1.10. Once 1.9 support is
  discontinued, this function may simply be removed."
  [sym]
  (try
    (clojure.core/resolve sym)
    (catch Exception _
      nil)))

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
