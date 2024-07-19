(ns orchard.print-test
  (:require
   [clojure.test :as t :refer [is are deftest testing]]
   [orchard.print :as sut])
  (:import
   (mx.cider.orchard TruncatingStringWriter
                     TruncatingStringWriter$TotalLimitExceeded)))

;; for `match?`
(require 'matcher-combinators.test)

(def long-nested-coll (vec (map #(range (* % 10) (+ (* % 10) 80)) (range 200))))
(def graph-with-loop (let [a (java.util.HashMap.)
                           b (java.util.HashMap.)]
                       (.put a :b b)
                       (.put b :a a)
                       a))
(def infinite-map (let [m (java.util.HashMap.)]
                    (.put m (symbol "long key to ensure total length works accurately") m)
                    m))

(defn nasty [lvl]
  (if (= lvl 0)
    1234
    (into {} (map (fn [k]
                    [k (nasty (dec lvl))])
                  (map (comp keyword str char) (range (int \a) (int \n)))))))

(defprotocol IMyTestType
  (^String get-name [this]))

(deftype MyTestType [name]
  IMyTestType
  (get-name [_this] name))

(defmethod sut/print MyTestType [obj ^java.io.Writer w]
  (.write w (str "#<MyTestType " (get-name obj) ">")))

(defrecord TestRecord [a b c d])

(defn sample-writer [atom-limit total-limit & strings]
  (let [writer (TruncatingStringWriter. atom-limit total-limit)]
    (try (run! #(.write writer ^String %) strings)
         (catch TruncatingStringWriter$TotalLimitExceeded _))
    (str writer)))

(deftest truncating-string-writer
  (are [result strings] (match? result (apply sample-writer 5 20 strings))
    "1" ["1"]
    "hello" ["hello"]
    "longs..." ["longstring"]
    "longs...longs...tail" ["longstring" "longstring" "tail"]
    "longs...longs...unfi..." ["longstring" "longstring" "unfit"]
    "hellohellohellohello" ["hello" "hello" "hello" "hello"]
    "hellohellohellohello..." ["hello" "hello" "hello" "hello" "hello"]
    "hellohellohihihihell..." ["hello" "hello" "hi" "hi" "hi" "hello"]
    "" ["" "" "" "" ""]))

(deftest print-no-limits
  (are [result form] (match? result (sut/print-str form))
    "1" 1
    "\"2\"" "2"
    "\"special \\\" \\\\ symbols\"" "special \" \\ symbols"
    ":foo" :foo
    ":abc/def" :abc/def
    "sym" 'sym
    "(:a :b :c)" '(:a :b :c)
    "[1 2 3]" [1 2 3]
    "{:a 1, :b 2}" {:a 1 :b 2}
    "{}" {}
    "{}" (java.util.HashMap.)
    "#{:a}" #{:a}
    "(1 2 3)" (lazy-seq '(1 2 3))
    "(1 1 1 1 1)" (java.util.ArrayList. ^java.util.Collection (repeat 5 1))
    "{:a 1, :b 2}" (let [^java.util.Map x {:a 1 :b 2}]
                     (java.util.HashMap. x))
    "{:a 1, :b 2, :c 3, :d 4}" (->TestRecord 1 2 3 4)
    "long[] {1, 2, 3, 4}" (long-array [1 2 3 4])
    "long[] {}" (long-array [])
    "java.lang.Long[] {0, 1, 2, 3, 4}" (into-array Long (range 5))
    "java.lang.Long[] {}" (into-array Long [])
    "#<MyTestType test1>" (MyTestType. "test1")
    "#Atom[1]" (atom 1)
    "#Delay[<pending>]" (delay 1)
    "#Delay[1]" (doto (delay 1) deref)
    "#Delay[<failed>]" (let [d (delay (/ 1 0))] (try @d (catch Exception _)) d)
    "#function[clojure.core/str]" str))

(deftest print-writer-limits
  (testing "global writer limits will stop the printing when reached"
    (are [result form] (= result (binding [sut/*max-atom-length* 10
                                           sut/*max-total-length* 30]
                                   (sut/print-str form)))
      "\"aaaaaaaaaa...\"" (apply str (repeat 300 "a"))
      "[\"aaaaaaaaaa...\" \"aaaaaaaaaa....." [(apply str (repeat 300 "a")) (apply str (repeat 300 "a"))]
      "(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1..." (repeat 1)
      "[(1 1 1 1 1 1 1 1 1 1 1 1 1 1 ..." [(repeat 1)]
      "{:a {(0 1 2 3 4 5 6 7 8 9) 1, ..." {:a {(range 10) 1, 2 3, 4 5, 6 7, 8 9, 10 11}}
      "(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1..." (java.util.ArrayList. ^java.util.Collection (repeat 100 1))
      "java.lang....[] {0, 1, 2, 3, 4..." (into-array Long (range 10))
      "{:m {:m {:m {:m {:m 1234, :e 1..." (nasty 5)
      "{:b {:a {:b {:a {:b {:a {:b {:..." graph-with-loop))

  (testing "writer won't go much over total-length"
    (is (= 2003 (count (binding [sut/*max-total-length* 2000]
                         (sut/print-str infinite-map)))))))

(deftest print-clojure-limits
  (are [result form] (= result (binding [*print-length* 5
                                         *print-level* 3]
                                 (sut/print-str form)))
    "(1 1 1 1 1 ...)" (repeat 1)
    "[(1 1 1 1 1 ...)]" [(repeat 1)]
    "{:a {(0 1 2 3 4 ...) 1, 2 3, 4 5, 6 7, 8 9, ...}}" {:a {(range 10) 1, 2 3, 4 5, 6 7, 8 9, 10 11}}
    "(1 1 1 1 1 ...)" (java.util.ArrayList. ^java.util.Collection (repeat 100 1))
    "(0 1 2 3 4 ...)" (eduction (range 10))
    "java.lang.Long[] {0, 1, 2, 3, 4, ...}" (into-array Long (range 10))
    "{:b {:a {:b {...}}}}" graph-with-loop)

  (are [result form] (= result (binding [*print-length* 3
                                         *print-level* 2]
                                 (sut/print-str form)))
    "{:a {(...) 1, 2 3, 4 5, ...}}" {:a {(range 10) 1, 2 3, 4 5, 6 7, 8 9, 10 11}}
    "[(0 1 2 ...) (10 11 12 ...) (20 21 22 ...) ...]" long-nested-coll
    "{:m {:m {...}, :e {...}, :l {...}, ...}, :e {:m {...}, :e {...}, :l {...}, ...}, :l {:m {...}, :e {...}, :l {...}, ...}, ...}" (nasty 5))

  (are [result lvl] (= result (binding [*print-level* lvl]
                                (sut/print-str (atom {:a (range 10)}))))
    "#Atom[...]" 0
    "#Atom[{...}]" 1
    "#Atom[{:a (...)}]" 2
    "#Atom[{:a (0 1 2 3 4 5 6 7 8 9)}]" 3))
