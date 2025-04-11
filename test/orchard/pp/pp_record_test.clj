(ns orchard.pp.pp-record-test
  (:require [clojure.test :refer [deftest is]]
            [orchard.pp.test :refer [pp replace-crlf]]))

(defrecord R [x])

(deftest pprint-record
  ;; unlike pr, clojure.pprint doesn't print records with the
  ;; fully-qualified record name in the prefix.
  (is (= (replace-crlf (with-out-str (prn (->R 1)))) (pp (->R 1))))

  (is (= "#orchard.pp.pp_record_test.R{:x
                             {:a
                              1,
                              :b
                              2,
                              :c
                              3,
                              :d
                              4}}
"
         (pp (->R {:a 1 :b 2 :c 3 :d 4}) :max-width 31))))
