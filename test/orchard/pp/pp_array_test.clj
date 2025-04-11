(ns orchard.pp.pp-array-test
  (:require [clojure.test :refer [deftest is]]
            [orchard.pp.test :refer [pp]]))

(deftest pprint-array
  (is (= "[true false]\n" (pp (boolean-array [true false]))))
  (is (= "[97 98]\n" (pp (byte-array [(int \a) (int \b)]))))
  (is (= "[\\a \\b]\n" (pp (char-array [\a \b]))))
  (is (= "[1.0 2.0]\n" (pp (double-array [1.0 2.0]))))
  (is (= "[3.0 4.0]\n" (pp (float-array [3.0 4.0]))))
  (is (= "[1 2 3]\n" (pp (int-array [1 2 3]))))
  (is (= "[4 5 6]\n" (pp (into-array [4 5 6]))))
  (is (= "[7 8 9]\n" (pp (long-array [7 8 9]))))
  (is (= "[{:a 1} {:b 2}]\n" (pp (object-array [{:a 1} {:b 2}]))))
  (is (= "[10 11 22]\n" (pp (short-array [10 11 22]))))
  (is (= "[[1 2 3] [4 5 6]]\n" (pp (to-array-2d [[1 2 3] [4 5 6]])))))
