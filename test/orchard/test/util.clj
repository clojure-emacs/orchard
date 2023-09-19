(ns orchard.test.util
  (:require [clojure.java.io :as io]))

(def has-enriched-classpath?
  (boolean (or (io/resource "java/lang/Thread.java")
               (io/resource "java.base/java/lang/Thread.java"))))
