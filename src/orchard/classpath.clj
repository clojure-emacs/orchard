(ns orchard.classpath
  "A simple wrapper around `clojure.java.classpath` that is Boot-aware."
  (:require [clojure.java.classpath :as cp]
            [clojure.string :as str]
            [orchard.classloader :as cl]
            [orchard.misc :as u])
  (:import java.io.File
           java.util.jar.JarFile))

(defn classpath
  "Return a sequence of File objects of elements on the classpath.

  It takes into account the classpath trickery performed by Boot."
  ([]
   (classpath (cl/class-loader)))
  ([classloader]
   (let [sep (re-pattern File/pathSeparator)
         ;; TODO: Check if that's really needed - after all we have a Boot-aware classloader
         boot-classpath (u/boot-fake-classpath)
         path (if boot-classpath
                (map #(File. ^String %) (str/split boot-classpath sep))
                ;; See https://dev.clojure.org/jira/browse/CLASSPATH-8
                (or (seq (cp/classpath classloader))
                    ;; Java 9+
                    (cp/system-classpath)))]
     path)))

(defn classpath-directories
  "Returns a sequence of File objects for the directories on classpath.

  Uses `classpath` instead of `clojure.java.classpath/classpath`."
  ([]
   (classpath-directories (classpath)))
  ([path]
   (filter #(.isDirectory ^File %) path)))

(defn classpath-jarfiles
  "Returns a sequence of JarFile objects for the JAR files on classpath.

  Uses `classpath` instead of `clojure.java.classpath/classpath`."
  ([]
   (classpath-jarfiles (classpath)))
  ([path]
   (map #(JarFile. ^File %) (filter cp/jar-file? path))))
