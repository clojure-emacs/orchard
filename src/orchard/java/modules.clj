(ns orchard.java.modules
  "Utilities for accessing module information. Requires JDK11 and onward.")

(defn module-name
  "Return the module name for the class if it can be resolved."
  [klass]
  (some-> klass ^Class resolve .getModule .getName))
