(ns orchard.inspect-test
  (:require
   [clojure.test :refer :all]
   [orchard.inspect :as inspect]))

(def nil-result ["(\"nil\" (:newline))"])

(def var-result ["(\"Class\" \": \" (:value \"clojure.lang.Var\" 0) (:newline) \"Meta Information: \" (:newline) \"  \" (:value \":ns\" 1) \" = \" (:value \"clojure.core\" 2) (:newline) \"  \" (:value \":name\" 3) \" = \" (:value \"*assert*\" 4) (:newline) \"Value: \" (:value \"true\" 5))"])

(def code "(sorted-map :a {:b 1} :c \"a\" :d 'e :f [2 3])")

(def eval-result (eval (read-string code)))

(def inspect-result ["(\"Class\" \": \" (:value \"clojure.lang.PersistentTreeMap\" 0) (:newline) \"Contents: \" (:newline) \"  \" (:value \":a\" 1) \" = \" (:value \"{ :b 1 }\" 2) (:newline) \"  \" (:value \":c\" 3) \" = \" (:value \"\\\"a\\\"\" 4) (:newline) \"  \" (:value \":d\" 5) \" = \" (:value \"e\" 6) (:newline) \"  \" (:value \":f\" 7) \" = \" (:value \"[ 2 3 ]\" 8) (:newline))"])

(def push-result ["(\"Class\" \": \" (:value \"clojure.lang.PersistentArrayMap\" 0) (:newline) \"Contents: \" (:newline) \"  \" (:value \":b\" 1) \" = \" (:value \"1\" 2) (:newline) (:newline) \"  Path: :a\")"])

(def inspect-result-with-nil ["(\"Class\" \": \" (:value \"clojure.lang.PersistentVector\" 0) (:newline) \"Contents: \" (:newline) \"  \" \"0\" \". \" (:value \"1\" 1) (:newline) \"  \" \"1\" \". \" (:value \"2\" 2) (:newline) \"  \" \"2\" \". \" (:value \"\" 3) (:newline) \"  \" \"3\" \". \" (:value \"3\" 4) (:newline))"])

(def java-hashmap-inspect-result ["(\"Class\" \": \" (:value \"java.util.HashMap\" 0) (:newline) \"Contents: \" (:newline) \"  \" (:value \":b\" 1) \" = \" (:value \"2\" 2) (:newline) \"  \" (:value \":c\" 3) \" = \" (:value \"3\" 4) (:newline) \"  \" (:value \":a\" 5) \" = \" (:value \"1\" 6) (:newline))"])

(def long-sequence (range 70))
(def long-vector (vec (range 70)))
(def long-map (zipmap (range 70) (range 70)))
(def long-nested-coll (vec (map #(range (* % 10) (+ (* % 10) 80)) (range 200))))

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
               (inspect/down 2)
               render)))))

(deftest pop-test
  (testing "popping a rendered expr inspector"
    (is (= inspect-result
           (-> eval-result
               inspect
               (inspect/down 2)
               inspect/up
               render)))))

(deftest pagination-test
  (testing "big collections are paginated"
    (is (= 33 (-> long-sequence
                  inspect
                  :counter)))
    ;; Twice more for maps
    (is (= 65 (-> long-map
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
               :current-page))))
  (testing "page numbers are tracked per nesting level"
    (let [ins (-> long-nested-coll
                  inspect
                  inspect/next-page
                  inspect/next-page
                  inspect/next-page
                  inspect/next-page)]
      (is (= 4 (:current-page ins)))
      (let [ins (-> ins
                    (inspect/down 1)
                    inspect/next-page
                    inspect/next-page)]
        (is (= 2 (:current-page ins)))
        (is (= 4 (:current-page (inspect/up ins))))))))

(deftest path-test
  (testing "inspector tracks the path in the data structure"
    (is (.endsWith (first (-> long-map
                              inspect
                              (inspect/down 39)
                              render))
                   "\"  Path: (find 50) key\")"))
    (is (.endsWith (first (-> long-map
                              inspect
                              (inspect/down 40)
                              render))
                   "\"  Path: (get 50)\")"))
    (is (.endsWith (first (-> long-map
                              inspect
                              (inspect/down 40)
                              (inspect/down 0)
                              render))
                   "\"  Path: (get 50) class\")")))
  (testing "doesn't show path if unknown navigation has happened"
    (is (.endsWith (first (-> long-map
                              inspect
                              (inspect/down 40)
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
      "{ :a { ( 0 1 2 3 4 ... ) 1, ... } }" {:a {(range 10) 1, 2 3, 4 5, 6 7, 8 9, 10 11}}
      "( 1 2 3 )" (lazy-seq '(1 2 3))
      "( 1 1 1 1 1 ... )" (java.util.ArrayList. (repeat 100 1))
      "( 1 2 3 )" (java.util.ArrayList. [1 2 3])
      "{ :a 1, :b 2 }" (java.util.HashMap. {:a 1 :b 2})
      "#<MyTestType test1>" (MyTestType. "test1"))))

(deftest inspect-coll-test
  (testing "inspect :coll prints contents of the coll"
    (is (= inspect-result-with-nil
           (render (inspect/start (inspect/fresh) [1 2 nil 3]))))))

(deftest inspect-java-hashmap-test
  (testing "inspecting java.util.Map descendendants prints a key-value coll"
    (is (= java-hashmap-inspect-result
           (render (inspect/start (inspect/fresh)
                                  (java.util.HashMap. {:a 1, :b 2, :c 3})))))))

(deftest inspect-path
  (testing "inspector keeps track of the path in the inspected structure"
    (let [t {:a (list 1 2 {:b {:c (vec (map (fn [x] {:foo (* x 10)}) (range 100)))}})
             :z 42}
          inspector (-> (inspect/start (inspect/fresh) t)
                        (inspect/down 1)
                        (inspect/up)
                        (inspect/down 2)
                        (inspect/down 2)
                        (inspect/up)
                        (inspect/down 3)
                        (inspect/down 2)
                        (inspect/down 2)
                        inspect/next-page
                        inspect/next-page
                        (inspect/down 10)
                        (inspect/down 1))]
      (is (= '[:a (nth 2) :b :c (nth 73) (find :foo) key] (:path inspector)))
      (is (= '[:a (nth 2) :b :c (nth 73) (find :foo) key class]
             (:path (-> inspector (inspect/down 0)))))
      (is (= '[:a (nth 2) :b :c (nth 73) (find :foo) key class <unknown>]
             (:path (-> inspector (inspect/down 0) (inspect/down 1))))))))
