(ns orchard.print-test
  (:require
   [clojure.test :as t :refer [is are deftest testing]]
   [orchard.print :as sut]
   [orchard.test.util :refer [is+]])
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
(defrecord EmptyRecord [])

;; A record and a java.util.Map that define their own `print-method` - like
;; `overtone.at-at`'s RecurringJob (#412) or `tech.ml.dataset` datasets (CIDER
;; #4088).  Orchard should render them with that method instead of traversing.
(defrecord PrintMethodRecord [id peer])
(defmethod print-method PrintMethodRecord [x ^java.io.Writer w]
  (.write w (str "#<PrintMethodRecord " (:id x) ">")))

;; Only the interfaces matter for these fixtures: values with a custom
;; print-method are never traversed, so no method bodies are needed.
(deftype PrintMethodMap []
  java.util.Map)
(defmethod print-method PrintMethodMap [_ ^java.io.Writer w]
  (.write w "#dataset[...]"))

;; The exact shape of a `tech.ml.dataset` dataset: implements both
;; java.util.Map and IPersistentMap, and defines its own `print-method`.
(deftype PrintMethodDataset []
  java.util.Map
  clojure.lang.IPersistentMap)
(defmethod print-method PrintMethodDataset [_ ^java.io.Writer w]
  (.write w "#dataset[5x2]"))

(deftype PrintMethodList []
  java.util.List)
(defmethod print-method PrintMethodList [_ ^java.io.Writer w]
  (.write w "#custom-list[...]"))

;; An IPersistentMap that does NOT implement java.util.Map. Orchard's :map
;; printer requires java.util.Map, so such types must print via print-method.
(deftype PurePersistentMap [^clojure.lang.IPersistentMap m]
  clojure.lang.IPersistentMap
  (seq [_] (seq m)) (count [_] (count m))
  (iterator [_] (.iterator ^Iterable m))
  (valAt [_ k] (get m k)) (valAt [_ k d] (get m k d))
  (equiv [this that] (identical? this that)))

;; Clojure collection types (not just Java ones) with their own print-method.
(deftype PrintMethodVector [v]
  clojure.lang.IPersistentVector)
(defmethod print-method PrintMethodVector [^PrintMethodVector x ^java.io.Writer w]
  (.write w (str "#custom-vec" (.-v x))))

(deftype PrintMethodSet [s]
  clojure.lang.IPersistentSet)
(defmethod print-method PrintMethodSet [^PrintMethodSet x ^java.io.Writer w]
  (.write w (str "#custom-set" (vec (.-s x)))))

;; Custom print-methods are arbitrary user code and may misbehave; orchard
;; must fall back to structural printing instead of crashing.
(defrecord ThrowingPrintRecord [id])
(defmethod print-method ThrowingPrintRecord [_ _]
  (throw (ex-info "boom" {})))

(defrecord RecursivePrintRecord [id peer])
(defmethod print-method RecursivePrintRecord [x ^java.io.Writer w]
  ;; Loops forever, like a print-method descending a cyclic value (#412).
  (print-method x w))

;; A java.util.Map whose print-method resolution is ambiguous (two matching
;; implementations, no preference). get-method throws for such values, but
;; printing them must survive that and fall back to structural.
(definterface AmbiguousPrintA)
(definterface AmbiguousPrintB)
(deftype AmbiguousPrintMap [^java.util.Map m]
  AmbiguousPrintA AmbiguousPrintB
  java.util.Map
  (get [_ k] (.get m k)) (size [_] (.size m)) (isEmpty [_] (.isEmpty m))
  (entrySet [_] (.entrySet m)) (keySet [_] (.keySet m)) (values [_] (.values m))
  (containsKey [_ k] (.containsKey m k)))
(defmethod print-method AmbiguousPrintA [_ ^java.io.Writer w] (.write w "A"))
(defmethod print-method AmbiguousPrintB [_ ^java.io.Writer w] (.write w "B"))

;; Same ambiguity for a List: the empty case takes a different code path
;; (print-coll delegates empty collections to print-method).
(deftype AmbiguousPrintList [^java.util.List l]
  AmbiguousPrintA AmbiguousPrintB
  java.util.List
  (size [_] (.size l)) (isEmpty [_] (.isEmpty l))
  (iterator [_] (.iterator l)))

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
    "\\space" \space
    "(:a :b :c)" '(:a :b :c)
    "[1 2 3]" [1 2 3]
    "{:a 1, :b 2}" {:a 1 :b 2}
    "[:a 1]" (first {:a 1 :b 2})
    "([:a 1] [:b 2])" (seq {:a 1 :b 2})
    "[[:a 1] [:b 2]]" (vec {:a 1 :b 2})
    "{}" {}
    "{}" (java.util.HashMap.)
    "#{:a}" #{:a}
    "(1 2 3)" (lazy-seq '(1 2 3))
    "(1 1 1 1 1)" (java.util.ArrayList. ^java.util.Collection (repeat 5 1))
    "{:a 1, :b 2}" (let [^java.util.Map x {:a 1 :b 2}]
                     (java.util.HashMap. x))
    "#TestRecord{:a 1, :b 2, :c 3, :d 4}" (->TestRecord 1 2 3 4)
    "#EmptyRecord{}" (->EmptyRecord)
    "long[] {1, 2, 3, 4}" (long-array [1 2 3 4])
    "long[] {}" (long-array [])
    "java.lang.Long[] {0, 1, 2, 3, 4}" (into-array Long (range 5))
    "java.lang.Long[] {}" (into-array Long [])
    "#<MyTestType test1>" (MyTestType. "test1")
    "#atom[1]" (atom 1)
    "#delay[<pending>]" (delay 1)
    "#delay[1]" (doto (delay 1) deref)
    #"#delay\[<failed> #error\[java.lang.ArithmeticException \"Divide by zero\"" (let [d (delay (/ 1 0))] (try @d (catch Exception _)) d)
    "#promise[<pending>]" (promise)
    "#promise[1]" (doto (promise) (deliver 1))
    "#future[<pending>]" (future (Thread/sleep 10000))
    "#future[1]" (doto (future 1) deref)
    "#agent[1]" (agent 1)
    #"#error\[clojure.lang.ExceptionInfo \"Boom\" \"orchard.print_test.+\"\]" (ex-info "Boom" {})
    #"#error\[clojure.lang.ExceptionInfo \"Boom\" \{:a 1\} \"orchard.print_test.+\"\]" (ex-info "Boom" {:a 1})
    #"#error\[java.lang.RuntimeException \"Runtime!\" \"orchard.print_test.+\"\]" (RuntimeException. "Runtime!")
    #"#error\[java.lang.RuntimeException \"Outer: Inner\" \"orchard.print_test.+\"\]" (RuntimeException. "Outer"
                                                                                                         (RuntimeException. "Inner"))
    #"multifn\[print .+\]" sut/print
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
                         (sut/print-str infinite-map))))))

  (testing "doesn't stack overflow when printing self-referential collections"
    (is (= 5105 (count (sut/print-str infinite-map))))))

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
    "#atom[...]" 0
    "#atom[{...}]" 1
    "#atom[{:a (...)}]" 2
    "#atom[{:a (0 1 2 3 4 5 6 7 8 9)}]" 3))

(deftest print-non-iterable
  (is (= "#{1 2 3}" (sut/print-str (reify clojure.lang.IPersistentSet
                                     (equiv [t o] (.equals t o))
                                     (seq [_] (seq [1 2 3])))))))

(defmethod orchard.print/print ::custom-rec [_ w] (sut/print 'hello w))

(deftest print-custom-print-method
  (is (= "hello"
         (sut/print-str (with-meta (->TestRecord 1 2 3 4) {:type ::custom-rec})))))

(deftest honor-print-method-test
  (testing "records with a custom print-method are rendered with it, not structurally"
    (is (= "#<PrintMethodRecord 1>" (sut/print-str (->PrintMethodRecord 1 nil)))))

  (testing "Java collections with a custom print-method are rendered with it (CIDER #4088)"
    (is (= "#dataset[...]" (sut/print-str (PrintMethodMap.)))))

  (testing "types implementing both IPersistentMap and java.util.Map honor their print-method (tech.ml.dataset shape)"
    (is (= "#dataset[5x2]" (sut/print-str (PrintMethodDataset.)))))

  (testing "Java lists with a custom print-method are rendered with it"
    (is (= "#custom-list[...]" (sut/print-str (PrintMethodList.)))))

  (testing "a self-referential value uses its print-method instead of overflowing (#412)"
    (let [x (->PrintMethodRecord 2 nil)]
      (is (= "#<PrintMethodRecord 2>" (sut/print-str (assoc x :peer x))))))

  (testing "plain records and collections are still printed structurally"
    (is (= "#TestRecord{:a 1, :b 2, :c 3, :d 4}" (sut/print-str (->TestRecord 1 2 3 4))))
    (is (= "{:a 1}" (sut/print-str {:a 1})))
    (is (= "[1 2 3]" (sut/print-str [1 2 3])))
    (is (= "{\"a\" 1}" (sut/print-str (java.util.HashMap. {"a" 1}))))
    (is (= "(1 2 3)" (sut/print-str (java.util.ArrayList. [1 2 3])))))

  (testing "Clojure vector and set types with a custom print-method are rendered with it"
    (is (= "#custom-vec[1 2 3]" (sut/print-str (PrintMethodVector. [1 2 3]))))
    (is (= "#custom-set[1 2 3]" (sut/print-str (PrintMethodSet. (sorted-set 1 2 3))))))

  (testing "IPersistentMap-only types still print via their print-method"
    (is (= "{:a 1}" (sut/print-str (PurePersistentMap. {:a 1})))))

  (testing "a throwing print-method falls back to structural printing"
    (is (= "#ThrowingPrintRecord{:id 1}" (sut/print-str (->ThrowingPrintRecord 1)))))

  (testing "a print-method that overflows the stack falls back to structural printing (#412)"
    (is (= "#RecursivePrintRecord{:id 1, :peer nil}"
           (sut/print-str (->RecursivePrintRecord 1 nil)))))

  (testing "ambiguous print-method implementations don't break printing"
    (is (= "{\"a\" 1}"
           (sut/print-str (AmbiguousPrintMap. (java.util.HashMap. {"a" 1})))))
    (is (= "(1 2 3)"
           (sut/print-str (AmbiguousPrintList. (java.util.ArrayList. [1 2 3])))))
    (is (= "()" (sut/print-str (AmbiguousPrintList. (java.util.ArrayList.))))))

  (testing "re-registering a generic print-method doesn't disable structural printing"
    (let [orig (get-method print-method java.util.Map)]
      (try
        ;; Simulates a namespace reload: a fresh fn is registered for the
        ;; same generic interface.
        (.addMethod ^clojure.lang.MultiFn print-method java.util.Map
                    (fn [m w] (orig m w)))
        (is (= "{\"a\" 1}" (sut/print-str (java.util.HashMap. {"a" 1}))))
        (finally
          (.addMethod ^clojure.lang.MultiFn print-method java.util.Map orig))))))

(deftest qualified-keywords-compaction
  (are [kw repr] (= repr (sut/print-str kw))
    :foo     ":foo"
    :foo/bar ":foo/bar"
    ::foo    ":orchard.print-test/foo"
    ::t/foo  ":clojure.test/foo")
  (is (= ":foo" (sut/print-str :foo)))
  (is (= ":foo/bar" (sut/print-str :foo/bar)))
  (is (= ":orchard.print-test/foo" (sut/print-str ::foo)))
  (is (= ":clojure.test/foo" (sut/print-str :clojure.test/foo)))

  (testing "binding *pov-ns* enables keyword compaction"
    (binding [sut/*pov-ns* (find-ns 'orchard.print-test)]
      (are [kw repr] (= repr (sut/print-str kw))
        :foo             ":foo"
        :foo/bar         ":foo/bar"
        ::foo            "::foo"
        ::t/foo          "::t/foo"
        :clojure.set/foo ":clojure.set/foo")))

  (testing "from other pov NS the printing will be different"
    (binding [sut/*pov-ns* (create-ns 'throwaway)]
      (are [kw repr] (= repr (sut/print-str kw))
        ::foo    ":orchard.print-test/foo"
        ::t/foo          ":clojure.test/foo"))))

(deftest broken-eduction-test
  (testing "shouldn't throw if printing an eduction that lacks Seq impl"
    (is+ #"\(\"<<java.lang.IllegalArgumentException: Don't know how to create ISeq from:"
         (sut/print-str (eduction (map identity)
                                  (reify clojure.core.protocols.CollReduce
                                    (coll-reduce [_ f]
                                      (reduce f (range 10)))
                                    (coll-reduce [_ f init]
                                      (reduce f init (range 10)))))))))

(deftest short-record-names-test
  (testing "*short-record-names* controls how records are printed"
    (is (= "#TestRecord{:a 1, :b 2, :c 3, :d 4}" (sut/print-str (->TestRecord 1 2 3 4))))
    (binding [sut/*short-record-names* false]
      (is (= "#orchard.print_test.TestRecord{:a 1, :b 2, :c 3, :d 4}"
             (sut/print-str (->TestRecord 1 2 3 4)))))))
