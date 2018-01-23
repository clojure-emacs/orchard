(ns orchard.classpath
  (:require [clojure.java.classpath :as cp]
            [clojure.string :as str]
            [orchard.classloader :as cl]
            [orchard.misc :as u])
  (:import java.io.File
           java.util.jar.JarFile))

(defn classpath
  "Return a sequence of File objects of elements on the classpath.

  Takes into account:

  - Classpath trickery performed by Boot
  - Java 9's boot classloader no longer being an instance of URLClassLoader"
  ([]
   (classpath (cl/class-loader)))
  ([classloader]
   (let [sep (re-pattern File/pathSeparator)
         boot-classpath (u/boot-fake-classpath)
         path (cond
                boot-classpath (str/split boot-classpath sep)
                (neg? (compare u/java-api-version "9")) (map str (cp/classpath classloader))
                :else (-> (System/getProperty "java.class.path")
                          (str/split sep)))]
     (map #(File. %) path))))

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
