(ns orchard.java.modules
  "Utilities for accessing module information. Requires JDK11 and onward.")

(defn module-name
  "Return the module name for the class."
  [class-or-sym]
  (let [^Class klass (if (symbol? class-or-sym)
                       (resolve class-or-sym)
                       class-or-sym)]
    (some-> klass .getModule .getName)))
