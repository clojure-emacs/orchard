(ns orchard.pp.pp-print-dup-test
  (:require [clojure.test :refer [deftest is]]
            [orchard.pp.test :refer [pp]]))

(defrecord R [x])

(deftest pprint-dup
  (binding [*print-dup* true]
    (doseq [x [1
               1.0
               1N
               1M
               "foo"
               {:a 1}
               [:a :b :c]
               #{:a :b :c}
               (java.math.BigInteger. "1")
               #'map
               #(inc 1)
               (doto (java.util.HashMap.) (.put :a 1))
               \a
               1/2
               #"[a-z]"
               (find-ns 'user)
               (java.util.Date.)
               (java.util.UUID/randomUUID)
               (->R 1)]]
      (is (= (str (print-str x) \newline) (pp x))))))
