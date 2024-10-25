(ns orchard.java.parser-utils
  "The common parts to the `parser` and `parser-next` namespaces."
  {:added "0.15.0"}
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [orchard.java.modules :as modules])
  (:import
   (java.io StringWriter)
   (javax.lang.model.element VariableElement)
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
(def result (atom nil))

(defn parse-java
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

(defn parse-variable-element [^VariableElement f env]
  {:name (-> f .getSimpleName str symbol)
   :type (-> f .asType (typesym env))})

;; Left for backward compatibility, callers are free to invoke
;; `orchard.java.modules/module-name` directly.
(defn module-name
  "Return the module name, or nil if modular"
  [klass]
  (modules/module-name klass))

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
