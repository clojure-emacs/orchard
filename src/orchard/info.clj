(ns orchard.info
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.java.javadoc :as javadoc]
            [orchard.classloader :refer [class-loader]]
            [orchard.java :as java]
            [orchard.misc :as u]
            [orchard.meta :as m]
            [orchard.spec :as spec]))

(def see-also-data
  (edn/read-string (slurp (io/resource "see-also.edn"))))

(defn see-also
  [ns sym]
  (let [var-key (str ns "/" sym)]
    (->> (get see-also-data var-key)
         (filter (comp resolve u/as-sym)))))

(defn info
  [ns sym]
  (or
   ;; it's a special (special-symbol?)
   (m/special-sym-meta sym)
   ;; it's a var
   (m/var-meta (m/resolve-var ns sym))
   ;; sym is an alias for another ns
   (m/ns-meta (get (m/resolve-aliases ns) sym))
   ;; it's simply a full ns
   (m/ns-meta (find-ns sym))
   ;; it's a Java class/member symbol...or nil
   (java/resolve-symbol ns sym)))

(defn info-java
  [class member]
  (java/member-info class member))
