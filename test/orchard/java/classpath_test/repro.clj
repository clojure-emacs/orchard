(ns orchard.java.classpath-test.repro
  (:require
   [clojure.java.classpath]
   [clojure.test :refer [are deftest is join-fixtures testing use-fixtures]]))

(deftest works
  (is (seq (clojure.java.classpath/classpath-directories))
      "This test should pass")

  (require 'orchard.java)
  @@(resolve 'orchard.java/cache-initializer)

  (is (seq (clojure.java.classpath/classpath-directories))
      "This test is expected to fail, because `orchard.java` performs side-effects that affect classloading matters."))
