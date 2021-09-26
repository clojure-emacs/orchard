(ns orchard.java.classpath-test.third-party-compat-test
  (:require
   [orchard.java]
   [clojure.java.classpath]
   [clojure.test :refer [deftest is]]))

;; make this namespace's tests deterministic:
@orchard.java/cache-initializer

(deftest works
  (is (seq (clojure.java.classpath/classpath-directories))
      "The presence of `clojure.java` does not affect third-party libraries"))
