(ns orchard.java.compatibility
  "Small utilities that unify code to work between different Java versions."
  (:require [orchard.misc :as misc])
  (:import java.lang.reflect.Field))

(defmacro ^:private module-name-macro [class-or-sym]
  ;; On JDK8, always return nil.
  (when (>= misc/java-api-version 11)
    (let [klass (with-meta (gensym "klass") {:tag `Class})]
      `(let [~klass (cond-> ~class-or-sym
                      (symbol? ~class-or-sym) resolve)]
         (some-> ~klass .getModule .getName)))))

(defn module-name
  "Return the module name for the class."
  [class-or-sym]
  (module-name-macro class-or-sym))

(defmacro get-field-value-macro [field obj]
  (if (>= misc/java-api-version 11)
    `(try (if (or (.canAccess ~field ~obj)
                  (.trySetAccessible ~field))
            (.get ~field ~obj)
            ::access-denied)
          (catch Exception ~'_ ::access-denied))
    ;; Fallback to deprecated try-catch based flow on JDK8.
    `(try (when-not (.isAccessible ~field)
            (.setAccessible ~field true))
          (.get ~field ~obj)
          (catch Exception ~'_ ::access-denied))))

(defn get-field-value [^Field field, obj]
  (get-field-value-macro field obj))
