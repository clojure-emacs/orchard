(ns orchard.pp.pp-custom-type-test
  (:require [clojure.pprint :as cpp]
            [clojure.test :refer [deftest is]]
            [orchard.pp.test :refer [pp replace-crlf]]))

(deftype T
         [^clojure.lang.Associative xs]
  clojure.lang.Associative
  (assoc [_ k v]
    (T. (.assoc xs k v))))

(def obj-re
  #"#object\[orchard.pp.pp_custom_type_test.T 0[xX][0-9a-fA-F]+ \"orchard.pp.pp_custom_type_test.T@[0-9a-fA-F]+\"\]\n")

(deftest pprint-custom-type
  (is (re-matches obj-re (replace-crlf (with-out-str (prn (T. {:a 1}))))))
  (is (re-matches obj-re (replace-crlf (with-out-str (cpp/pprint (T. {:a 1}))))))
  (is (re-matches obj-re (pp (T. {:a 1}))))
  (binding [*print-level* 0]
    (is (re-matches obj-re (pp (T. {:a 1}))))))
