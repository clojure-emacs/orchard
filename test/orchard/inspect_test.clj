(ns orchard.inspect-test
  (:require [orchard.inspect :as inspect]
            [clojure.test :refer :all]))

(def nil-result ["(\"nil\" (:newline))"])

(def var-result ["(\"Class\" \": \" (:value \"clojure.lang.Var\" 0) (:newline) \"Meta Information: \" (:newline) \"  \" (:value \":ns\" 1) \" = \" (:value \"clojure.core\" 2) (:newline) \"  \" (:value \":name\" 3) \" = \" (:value \"*assert*\" 4) (:newline) \"Value: \" (:value \"true\" 5))"])

(def code "(sorted-map :a {:b 1} :c \"a\" :d 'e :f [2 3])")

(def eval-result (eval (read-string code)))

(def inspect-result ["(\"Class\" \": \" (:value \"clojure.lang.PersistentTreeMap\" 0) (:newline) \"Contents: \" (:newline) \"  \" \"0\" \". \" (:value \"[ :a { :b 1 } ]\" 1) (:newline) \"  \" \"1\" \". \" (:value \"[ :c \\\"a\\\" ]\" 2) (:newline) \"  \" \"2\" \". \" (:value \"[ :d e ]\" 3) (:newline) \"  \" \"3\" \". \" (:value \"[ :f [ 2 3 ] ]\" 4) (:newline))"])

(def push-result ["(\"Class\" \": \" (:value \"clojure.lang.PersistentTreeMap$BlackVal\" 0) (:newline) \"Contents: \" (:newline) \"  \" \"0\" \". \" (:value \":a\" 1) (:newline) \"  \" \"1\" \". \" (:value \"{ :b 1 }\" 2) (:newline) (:newline) \"  Path: (find :a)\")"])

(def long-sequence (range 70))
(def long-vector (vec (range 70)))
(def long-map (zipmap (range 70) (range 70)))

(defn inspect
  [value]
  (inspect/start (inspect/fresh) value))

(defn render
  [inspector]
  (vector (pr-str (:rendered inspector))))

(deftest nil-test
  (testing "nil renders correctly"
    (is (= nil-result
           (-> nil
               inspect
               render)))))

(deftest pop-empty-test
  (testing "popping an empty inspector renders nil"
    (is (= nil-result
           (-> (inspect/fresh)
               inspect/up
               render)))))

(deftest pop-empty-idempotent-test
  (testing "popping an empty inspector is idempotent"
    (is (= nil-result
           (-> (inspect/fresh)
               inspect/up
               inspect/up
               render)))))

(deftest push-empty-test
  (testing "pushing an empty inspector index renders nil"
    (is (= nil-result
           (-> (inspect/fresh)
               (inspect/down 1)
               render)))))

(deftest push-empty-idempotent-test
  (testing "pushing an empty inspector index is idempotent"
    (is (= nil-result
           (-> (inspect/fresh)
               (inspect/down 1)
               (inspect/down 1)
               render)))))

(deftest inspect-var-test
  (testing "rendering a var"
    (is (= var-result
           (-> #'*assert*
               inspect
               render)))))

(deftest inspect-expr-test
  (testing "rendering an expr"
    (is (= inspect-result
           (-> eval-result
               inspect
               render)))))

(deftest push-test
  (testing "pushing a rendered expr inspector idx"
    (is (= push-result
           (-> eval-result
               inspect
               (inspect/down 1)
               render)))))

(deftest pop-test
  (testing "popping a rendered expr inspector"
    (is (= inspect-result
           (-> eval-result
               inspect
               (inspect/down 1)
               inspect/up
               render)))))

(deftest pagination-test
  (testing "big collections are paginated"
    (is (= 33 (-> long-sequence
                  inspect
                  :counter)))
    (is (= 33 (-> long-map
                  inspect
                  :counter)))
    (is (.startsWith (-> long-vector
                         inspect
                         :rendered
                         last)
                     "  Page size:")))
  (testing "small collections are not paginated"
    (is (= '(:newline)
           (-> (range 10)
               inspect
               :rendered
               last))))
  (testing "changing page size"
    (is (= 21 (-> long-sequence
                  inspect
                  (inspect/set-page-size 20)
                  :counter)))
    (is (= '(:newline) (-> long-sequence
                           inspect
                           (inspect/set-page-size 200)
                           :rendered
                           last))))
  (testing "uncounted collections have their size determined on the last page"
    (is (= "  Page size: 32, showing page: 2 of 2"
           (-> (range 50)
               inspect
               inspect/next-page
               :rendered
               last))))
  (testing "next-page and prev-page are bound to collection size"
    (is (= 2
           (-> long-vector
               inspect
               inspect/next-page
               inspect/next-page
               inspect/next-page
               inspect/next-page
               inspect/next-page
               :current-page)))
    (is (= 0
           (-> long-vector
               inspect
               inspect/prev-page
               inspect/prev-page
               :current-page)))
    (is (= 1
           (-> long-vector
               inspect
               inspect/next-page
               inspect/next-page
               inspect/prev-page
               inspect/next-page
               inspect/prev-page
               :current-page)))))

(deftest path-test
  (testing "inspector tracks the path in the data structure"
    (is (.endsWith (first (-> long-map
                              inspect
                              (inspect/down 20)
                              render))
                   "\"  Path: (find 50)\")"))
    (is (.endsWith (first (-> long-map
                              inspect
                              (inspect/down 20)
                              (inspect/down 1)
                              render))
                   "\"  Path: (find 50) first\")"))
    (is (.endsWith (first (-> long-map
                              inspect
                              (inspect/down 20)
                              (inspect/down 2)
                              render))
                   "\"  Path: (get 50)\")"))
    (is (.endsWith (first (-> long-map
                              inspect
                              (inspect/down 20)
                              (inspect/down 2)
                              (inspect/down 0)
                              render))
                   "\"  Path: (get 50) class\")")))
  (testing "doesn't show path if unknown navigation has happened"
    (is (.endsWith (first (-> long-map
                              inspect
                              (inspect/down 20)
                              (inspect/down 2)
                              (inspect/down 0)
                              (inspect/down 1)
                              render))
                   "(:newline))")))
  (testing "doesn't show the path in the top level"
    (is (.endsWith (first (-> [1 2 3]
                              inspect
                              render))
                   "(:newline))"))))

(defprotocol IMyTestType
  (^String get-name [this]))

(deftype MyTestType [name]
  IMyTestType
  (get-name [this] name))

(defmethod inspect/inspect-value MyTestType [obj]
  (str "#<MyTestType " (get-name obj) ">"))

(deftest inspect-val-test
  (testing "inspect-value print types"
    (are [result form] (= result (inspect/inspect-value form))
      "1" 1
      "\"2\"" "2"
      ":foo" :foo
      ":abc/def" :abc/def
      "( :a :b :c )" '(:a :b :c)
      "[ 1 2 3 ]" [1 2 3]
      "{ :a 1, :b 2 }" {:a 1 :b 2}
      "#{ :a }" #{:a}
      "( 1 1 1 1 1 ... )" (repeat 1)
      "[ ( 1 1 1 1 1 ... ) ]" [(repeat 1)]
      "{ :a { ( 0 1 2 3 4 ... ) 1, ... } }" {:a {(range 10) 1, 2 3, 4 5, 6 7, 8 9}}
      "( 1 2 3 )" (lazy-seq '(1 2 3))
      "#<MyTestType test1>" (MyTestType. "test1"))))

(deftest inspect-path
  (testing "inspector keeps track of the path in the inspected structure"
    (let [t {:a (list 1 2 {:b {:c (vec (map (fn [x] {:foo (* x 10)}) (range 100)))}})
             :z 42}
          inspector (-> (inspect/start (inspect/fresh) t)
                        (inspect/down 1) (inspect/down 2)
                        (inspect/up) (inspect/up)
                        (inspect/down 1) (inspect/down 2)
                        (inspect/down 2)
                        (inspect/up)
                        (inspect/down 3)
                        (inspect/down 1) (inspect/down 2)
                        (inspect/down 1) (inspect/down 2)
                        (inspect/down 10)
                        (inspect/down 1) (inspect/down 1))]
      (is (= '[:a (nth 2) :b :c (nth 9) (find :foo) first] (:path inspector)))
      (is (= '[:a (nth 2) :b :c (nth 9) (find :foo) first class]
             (:path (-> inspector (inspect/down 0)))))
      (is (= '[:a (nth 2) :b :c (nth 9) (find :foo) first class <unknown>]
             (:path (-> inspector (inspect/down 0) (inspect/down 1))))))))
