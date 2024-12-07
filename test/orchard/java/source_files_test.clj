(ns orchard.java.source-files-test
  (:require [clojure.test :refer [deftest is]]
            [orchard.java.source-files :as src-files]))

(deftest class->source-file-url-test
  (is (src-files/class->source-file-url mx.cider.orchard.LruMap)) ;; classpath
  (is (src-files/class->source-file-url Thread)) ;; JDK
  (is (src-files/class->source-file-url clojure.lang.PersistentVector)) ;; Clojure
  (is (nil? (src-files/class->source-file-url clojure.core.Eduction)))) ;; record
