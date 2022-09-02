(ns orchard.inspect-test
  (:require
   [clojure.data :as data]
   [clojure.walk :as walk]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [clojure.test :as t :refer [deftest is are testing]]
   [orchard.inspect :as inspect]
   [orchard.misc :refer [datafy? java-api-version]])
  (:import java.io.File))

(defn- demunge-str [s]
  (str/replace s #"(?i)\$([a-z-]+)__([0-9]+)(@[a-f0-9]+)?" "\\$$1"))

(defn- demunge
  ([rendered]
   (demunge rendered demunge-str))
  ([rendered demunge-fn]
   (walk/prewalk (fn [form]
                   (if (string? form)
                     (demunge-fn form)
                     form))
                 rendered)))

(defn- render-plain [x]
  (cond (and (seq? x) (keyword? (first x)))
        (let [[type value & args] x]
          (case type
            :newline "\n"
            :value (format "%s <%s>" value (str/join "," args))))
        (seq? x)
        (str/join "" (map render-plain x))
        :else (str x)))

(defn- diff-text [expected actual]
  (when (zero? (:exit (shell/sh "git" "--version")))
    (let [actual-file (File/createTempFile "actual" ".txt")
          expected-file (File/createTempFile "expected" ".txt")]
      (spit actual-file (render-plain actual))
      (spit expected-file (render-plain expected))
      (try (let [{:keys [exit out err] :as result}
                 (shell/sh "git" "diff"
                           (if (= "dumb" (System/getenv "TERM")) "--no-color" "--color")
                           "--minimal"
                           "--no-index"
                           (str actual-file) (str expected-file))]
             (case exit
               (0 1) out
               (ex-info "Failed to call diff" result)))
           (finally
             (io/delete-file actual-file)
             (io/delete-file expected-file))))))

(defn- test-message [msg expected actual]
  (let [expected-text (render-plain expected)
        actual-text (render-plain actual)]
    (with-out-str
      (println (format "Inspect test failed" (when msg (str ": " msg))))
      (let [diff (diff-text expected actual)]
        (when-not (str/blank? diff)
          (println)
          (println "=== Text Diff ===\n")
          (println diff)))
      (let [[only-in-expected only-in-actual both] (data/diff expected actual)]
        (when (seq only-in-expected)
          (println)
          (println "=== Expected data diff ===\n")
          (pprint/pprint only-in-expected))
        (when (seq only-in-actual)
          (println)
          (println "=== Actual data diff ===\n")
          (pprint/pprint only-in-actual)))
      (when-not (= expected-text actual-text)
        (when expected
          (println)
          (println "=== Expected text ===\n")
          (println expected-text))
        (when actual
          (println)
          (println "=== Actual text ===\n")
          (println actual-text)
          (println))))))

(defmethod t/assert-expr 'match? [msg form]
  `(let [expected# ~(nth form 1)
         actual# ~(nth form 2)
         result# (= expected# actual#)]
     (t/do-report
      {:type (if result# :pass :fail)
       :message (test-message ~msg expected# actual#)
       :expected expected#
       :actual actual#})
     result#))

(def nil-result
  '("nil" (:newline)))

(def code "(sorted-map :a {:b 1} :c \"a\" :d 'e :f [2 3])")

(def eval-result (eval (read-string code)))

(def inspect-result
  '("Class"
    ": "
    (:value "clojure.lang.PersistentTreeMap" 0)
    (:newline)
    (:newline)
    "--- Contents:"
    (:newline)
    "  " (:value ":a" 1) " = " (:value "{ :b 1 }" 2)
    (:newline)
    "  " (:value ":c" 3) " = " (:value "\"a\"" 4)
    (:newline)
    "  " (:value ":d" 5) " = " (:value "e" 6)
    (:newline)
    "  " (:value ":f" 7) " = " (:value "[ 2 3 ]" 8)
    (:newline)))

(-> (inspect/fresh)
    (inspect/start {:a {:b 1}}))

(-> (inspect/fresh)
    (inspect/start {:a {:b 1}})
    (inspect/down 1)
    (inspect/down 1))

(def long-sequence (range 70))
(def long-vector (vec (range 70)))
(def long-map (zipmap (range 70) (range 70)))
(def long-nested-coll (vec (map #(range (* % 10) (+ (* % 10) 80)) (range 200))))
(def truncated-string (str "\"" (apply str (repeat 146 "a")) "..."))

(defn- section? [name rendered]
  (when (string? rendered)
    (re-matches (re-pattern (format "--- %s:" name)) rendered)))

(defn- section [name rendered]
  (->> rendered
       (drop-while #(not (section? name %)))
       (take-while #(or (section? name %)
                        (not (section? ".*" %))))))

(defn- datafy-section [rendered]
  (section "Datafy" rendered))

(defn- header [rendered]
  (take-while #(not (and (string? %)
                         (re-matches #".*---.*" %))) rendered))

(defn- extend-datafy-class [m]
  (vary-meta m assoc 'clojure.core.protocols/datafy (fn [x] (assoc x :class (.getSimpleName (class x))))))

(defn- extend-nav-vector [m]
  (vary-meta m assoc 'clojure.core.protocols/nav (fn [coll k v] [k (get coll k v)])))

(defn inspect
  [value]
  (inspect/start (inspect/fresh) value))

(defn render
  [inspector]
  (:rendered inspector))

(deftest nil-test
  (testing "nil renders correctly"
    (is (match? nil-result
                (-> nil
                    inspect
                    render)))))

(deftest pop-empty-test
  (testing "popping an empty inspector renders nil"
    (is (match? nil-result
                (-> (inspect/fresh)
                    inspect/up
                    render)))))

(deftest pop-empty-idempotent-test
  (testing "popping an empty inspector is idempotent"
    (is (match? nil-result
                (-> (inspect/fresh)
                    inspect/up
                    inspect/up
                    render)))))

(deftest push-empty-test
  (testing "pushing an empty inspector index renders nil"
    (is (match? nil-result
                (-> (inspect/fresh)
                    (inspect/down 1)
                    render)))))

(deftest push-empty-idempotent-test
  (testing "pushing an empty inspector index is idempotent"
    (is (match? nil-result
                (-> (inspect/fresh)
                    (inspect/down 1)
                    (inspect/down 1)
                    render)))))

(deftest inspect-var-test
  (testing "inspecting a var"
    (let [rendered (-> #'*assert* inspect render)]
      (testing "renders the header"
        (is (match? '("Class"
                      ": "
                      (:value "clojure.lang.Var" 0)
                      (:newline)
                      "Value: "
                      (:value "true" 1)
                      (:newline)
                      (:newline))
                    (header rendered))))
      (testing "renders the meta information section"
        (is (match? (cond-> '("--- Meta Information:"
                              (:newline)
                              "  " (:value ":ns" 2) " = " (:value "clojure.core" 3)
                              (:newline)
                              "  " (:value ":name" 4) " = " (:value "*assert*" 5)
                              (:newline))
                      datafy? (concat ['(:newline)]))
                    (section "Meta Information" rendered))))
      (when datafy?
        (testing "renders the datafy section"
          (is (match? '("--- Datafy:"
                        (:newline)
                        "  " "0" ". " (:value "true" 6)
                        (:newline))
                      (datafy-section rendered))))))))

(deftest inspect-expr-test
  (testing "rendering an expr"
    (is (match? inspect-result
                (-> eval-result
                    inspect
                    render)))))

(deftest push-test
  (testing "pushing a rendered expr inspector idx"
    (is (match? '("Class"
                  ": " (:value "clojure.lang.PersistentArrayMap" 0)
                  (:newline)
                  (:newline)
                  "--- Contents:"
                  (:newline)
                  "  " (:value ":b" 1) " = " (:value "1" 2)
                  (:newline)
                  (:newline)
                  "--- Path:"
                  (:newline)
                  "  "
                  ":a")
                (-> eval-result inspect (inspect/down 2) render)))))

(deftest pop-test
  (testing "popping a rendered expr inspector"
    (is (match? inspect-result
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
    (is (-> long-vector
            inspect
            :rendered
            ^String (last)
            (.startsWith "Page size:"))))
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
    (is (= "Page size: 32, showing page: 2 of 2"
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

(deftest eval-and-inspect-test
  (testing "evaluate expr in the context of currently inspected value"
    (is (match? '("Class"
                  ": " (:value "java.lang.String" 0)
                  (:newline)
                  "Value: " "\"1001\""
                  (:newline)
                  (:newline)
                  "--- Print:"
                  (:newline)
                  "  " "1001"
                  (:newline))
                (-> eval-result
                    inspect
                    (inspect/down 2)
                    (inspect/down 2)
                    (inspect/eval-and-inspect "(str (+ v 1000))")
                    render)))))

(deftest def-value-test
  (testing "define var with the currently inspected value"
    (-> eval-result
        inspect
        (inspect/down 2)
        (inspect/down 2)
        (inspect/def-current-value *ns* "--test-val--"))
    (is (= 1 @(resolve '--test-val--)))))

(deftest path-test
  (testing "inspector tracks the path in the data structure"
    (is (= "(find 50) key" (-> long-map inspect (inspect/down 39) render last)))
    (is (= "(get 50)" (-> long-map inspect (inspect/down 40) render last)))
    (is (= "(get 50) class"  (-> long-map inspect (inspect/down 40) (inspect/down 0) render last))))
  (testing "doesn't show path if unknown navigation has happened"
    (is (= '(:newline)  (-> long-map inspect (inspect/down 40) (inspect/down 0) (inspect/down 1) render last))))
  (testing "doesn't show the path in the top level"
    (is (= '(:newline) (-> [1 2 3] inspect render last)))))

(defprotocol IMyTestType
  (^String get-name [this]))

(deftype MyTestType [name]
  IMyTestType
  (get-name [_this] name))

(defmethod inspect/inspect-value MyTestType [obj]
  (str "#<MyTestType " (get-name obj) ">"))

(deftest inspect-val-test
  (testing "inspect-value print types"
    (are [result form] (match? result (inspect/inspect-value form))
      "1" 1
      "\"2\"" "2"
      truncated-string (apply str (repeat 300 \a))
      ":foo" :foo
      ":abc/def" :abc/def
      "( :a :b :c )" '(:a :b :c)
      "[ 1 2 3 ]" [1 2 3]
      "{ :a 1, :b 2 }" {:a 1 :b 2}
      "#{ :a }" #{:a}
      "( 1 1 1 1 1 ... )" (repeat 1)
      "[ ( 1 1 1 1 1 ... ) ]" [(repeat 1)]
      "{ :a { ( 0 1 2 3 4 ... ) 1, 2 3, 4 5, 6 7, 8 9, ... } }" {:a {(range 10) 1, 2 3, 4 5, 6 7, 8 9, 10 11}}
      "( 1 2 3 )" (lazy-seq '(1 2 3))
      "( 1 1 1 1 1 ... )" (java.util.ArrayList. ^java.util.Collection (repeat 100 1))
      "( 1 2 3 )" (let [^java.util.Collection x [1 2 3]]
                    (java.util.ArrayList. x))
      "{ :a 1, :b 2 }" (let [^java.util.Map x {:a 1 :b 2}]
                         (java.util.HashMap. x))
      "long[] { 1, 2, 3, 4 }" (long-array [1 2 3 4])
      "java.lang.Long[] { 0, 1, 2, 3, 4, ... }" (into-array Long (range 10))
      "#<MyTestType test1>" (MyTestType. "test1")))

  (testing "inspect-value adjust length and size"
    (binding [inspect/*max-atom-length* 6
              inspect/*max-coll-size* 2]
      (are [result form] (match? result (inspect/inspect-value form))
        "1" 1
        "nil" nil
        "\"2\"" "2"
        ":ab..." :abc/def
        "( :a :b )" '(:a :b)
        "[ 1 2 ... ]" [1 2 3]
        "{ :a 1, :b 2 }" {:a 1 :b 2}
        "{ :a 1, :b 2, ... }" {:a 1 :b 2 :c 3}
        "{ :a 1, :b 2, ... }" (sorted-map :d 4 :b 2 :a 1 :c 3)
        "( 1 1 ... )" (repeat 1)
        "[ ( 1 1 ... ) ]" [(repeat 1)]
        "{ :a { ( 0 1 ... ) \"ab..., 2 3, ... } }" {:a {(range 10) "abcdefg", 2 3, 4 5, 6 7, 8 9, 10 11}}
        "java.lang.Long[] { 0, 1, ... }" (into-array Long (range 10))))
    (binding [inspect/*max-coll-size* 6]
      (are [result form] (match? result (inspect/inspect-value form))
        "[ ( 1 1 1 1 1 1 ... ) ]" [(repeat 1)]
        "{ :a { ( 0 1 2 3 4 5 ... ) 1, 2 3, 4 5, 6 7, 8 9, 10 11 } }" {:a {(range 10) 1, 2 3, 4 5, 6 7, 8 9, 10 11}}))))

(deftest inspect-class-fields-test
  (testing "inspecting a class with fields renders correctly"
    (is (match? (case java-api-version
                  (8 11)
                  '("--- Fields:"
                    (:newline)
                    "  " (:value "public static final java.lang.Boolean java.lang.Boolean.FALSE" 5)
                    (:newline)
                    "  " (:value "public static final java.lang.Boolean java.lang.Boolean.TRUE" 6)
                    (:newline)
                    "  " (:value "public static final java.lang.Class java.lang.Boolean.TYPE" 7)
                    (:newline)
                    (:newline))
                  '("--- Fields:"
                    (:newline)
                    "  " (:value "public static final java.lang.Boolean java.lang.Boolean.FALSE" 6)
                    (:newline)
                    "  " (:value "public static final java.lang.Boolean java.lang.Boolean.TRUE" 7)
                    (:newline)
                    "  " (:value "public static final java.lang.Class java.lang.Boolean.TYPE" 8)
                    (:newline)
                    (:newline)))
                (->> Boolean inspect render (section "Fields")))))
  (testing "inspecting a class without fields renders correctly"
    (is (-> Object inspect render (section "Fields") empty?))))

(deftest inspect-coll-test
  (testing "inspect :coll prints contents of the coll"
    (is (match? '("Class"
                  ": " (:value "clojure.lang.PersistentVector" 0)
                  (:newline)
                  (:newline)
                  "--- Contents:"
                  (:newline)
                  "  " "0" ". " (:value "1" 1)
                  (:newline)
                  "  " "1" ". " (:value "2" 2)
                  (:newline)
                  "  " "2" ". " (:value "nil" 3)
                  (:newline)
                  "  " "3" ". " (:value "3" 4)
                  (:newline))
                (render (inspect/start (inspect/fresh) [1 2 nil 3]))))))

(deftest inspect-coll-nav-test
  (testing "inspecting a collection extended with the Datafiable and Navigable protocols"
    (let [rendered (-> (->> (iterate inc 0)
                            (map #(hash-map :x %))
                            (map extend-datafy-class)
                            (map extend-nav-vector))
                       inspect (inspect/set-page-size 2) render)]
      (testing "renders the content section"
        (is (match? '("--- Contents:"
                      (:newline)
                      "  " "0" ". " (:value "{ :x 0 }" 1)
                      (:newline)
                      "  " "1" ". " (:value "{ :x 1 }" 2)
                      (:newline)
                      "  " "..."
                      (:newline)
                      (:newline))
                    (section "Contents" rendered))))
      (when datafy?
        (testing "renders the datafy section"
          (is (match? '("--- Datafy:"
                        (:newline)
                        "  " "0" ". " (:value "{ :class \"PersistentHashMap\", :x 0 }" 3)
                        (:newline)
                        "  " "1" ". " (:value "{ :class \"PersistentHashMap\", :x 1 }" 4)
                        (:newline)
                        "  " "..."
                        (:newline)
                        (:newline))
                      (datafy-section rendered)))))
      (testing "renders the page info section"
        (is (match? '("--- Page Info:"
                      (:newline)
                      "  " "Page size: 2, showing page: 1 of ?")
                    (section "Page Info" rendered)))))))

(deftest inspect-configure-length-test
  (testing "inspect respects :max-atom-length and :max-coll-size configuration"
    (is (match? '("Class"
                  ": "
                  (:value "clojure.lang.PersistentVector" 0)
                  (:newline)
                  (:newline)
                  "--- Contents:"
                  (:newline)
                  "  " "0" ". " (:value "[ 1... 2222 333 ... ]" 1)
                  (:newline))
                (render (-> (inspect/fresh)
                            (assoc :max-atom-length 4
                                   :max-coll-size 3)
                            (inspect/start [[111111 2222 333 44 5]])))))))

(deftest inspect-java-hashmap-test
  (testing "inspecting java.util.Map descendendants prints a key-value coll"
    (let [^java.util.Map the-map  {:a 1, :b 2, :c 3}]
      (is (match? '("Class"
                    ": "
                    (:value "java.util.HashMap" 0)
                    (:newline)
                    (:newline)
                    "--- Contents:"
                    (:newline)
                    "  " (:value ":b" 1) " = " (:value "2" 2)
                    (:newline)
                    "  " (:value ":c" 3) " = " (:value "3" 4)
                    (:newline)
                    "  " (:value ":a" 5) " = " (:value "1" 6)
                    (:newline))
                  (-> (inspect/fresh)
                      (inspect/start (java.util.HashMap. the-map))
                      render))))))

(deftest inspect-java-object-test
  (testing "inspecting any Java object prints its fields"
    (is (match? '("Class"
                  ": "
                  (:value "clojure.lang.TaggedLiteral" 0)
                  (:newline)
                  "Value" ": " (:value "\"#foo ()\"" 1)
                  (:newline)
                  (:newline)
                  "--- Fields:"
                  (:newline) "  " (:value "\"form\"" 2) " = " (:value "()" 3)
                  (:newline) "  " (:value "\"tag\"" 4) " = " (:value "foo" 5)
                  (:newline)
                  (:newline)
                  "--- Static fields:"
                  (:newline) "  " (:value "\"FORM_KW\"" 6) " = " (:value ":form" 7)
                  (:newline) "  " (:value "\"TAG_KW\"" 8) " = " (:value ":tag" 9)
                  (:newline))
                (render (inspect/start (inspect/fresh)
                                       (clojure.lang.TaggedLiteral/create 'foo ())))))))

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

(deftest inspect-object-class-test
  (testing "inspecting the java.lang.Object class"
    (let [rendered (-> Object inspect render)]
      (testing "renders the header section"
        (is (match? '("Class" ": " (:value "java.lang.Class" 0) (:newline) (:newline))
                    (header rendered))))
      (testing "renders the constructors section"
        (is (match? '("--- Constructors:"
                      (:newline)
                      "  " (:value "public java.lang.Object()" 1)
                      (:newline)
                      (:newline))
                    (section "Constructors" rendered))))
      (testing "renders the methods section"
        (is (match? (cond-> '("--- Methods:"
                              (:newline)
                              "  " (:value "public boolean java.lang.Object.equals(java.lang.Object)" 2)
                              (:newline)
                              "  " (:value "public final native java.lang.Class java.lang.Object.getClass()" 3)
                              (:newline)
                              "  " (:value "public native int java.lang.Object.hashCode()" 4)
                              (:newline)
                              "  " (:value "public final native void java.lang.Object.notify()" 5)
                              (:newline)
                              "  " (:value "public final native void java.lang.Object.notifyAll()" 6)
                              (:newline)
                              "  " (:value "public java.lang.String java.lang.Object.toString()" 7)
                              (:newline)
                              "  " (:value "public final native void java.lang.Object.wait(long) throws java.lang.InterruptedException" 8)
                              (:newline)
                              "  " (:value "public final void java.lang.Object.wait() throws java.lang.InterruptedException" 9)
                              (:newline)
                              "  " (:value "public final void java.lang.Object.wait(long,int) throws java.lang.InterruptedException" 10)
                              (:newline))
                      datafy? (concat ['(:newline)]))
                    (section "Methods" rendered))))
      (when datafy?
        (testing "renders the datafy section"
          (is (match? `("--- Datafy:"
                        (:newline)
                        "  " (:value ":flags" 11) " = " (:value "#{ :public }" 12)
                        (:newline)
                        "  " (:value ":members" 13) " = "
                        (:value ~(str "{ clone [ { :name clone, :return-type java.lang.Object, :declaring-class java.lang.Object, "
                                      ":parameter-types [], :exception-types [ java.lang.CloneNotSupportedException ], ... } ], equals "
                                      "[ { :name equals, :return-type boolean, :declaring-class java.lang.Object, :parameter-types "
                                      "[ java.lang.Object ], :exception-types [], ... } ], finalize [ { :name finalize, :return-type void, "
                                      ":declaring-class java.lang.Object, :parameter-types [], :exception-types [ java.lang.Throwable ], "
                                      "... } ], getClass [ { :name getClass, :return-type java.lang.Class, :declaring-class java.lang.Object, "
                                      ":parameter-types [], :exception-types [], ... } ], hashCode [ { :name hashCode, :return-type int, "
                                      ":declaring-class java.lang.Object, :parameter-types [], :exception-types [], ... } ], ... }") 14)
                        (:newline)
                        "  " (:value ":name" 15) " = " (:value "java.lang.Object" 16)
                        (:newline))
                      (datafy-section rendered))))))))

(deftest inspect-atom-test
  (testing "inspecting an atom"
    (let [rendered (-> (atom {:a 1}) inspect render)]
      (testing "renders the header section"
        (is (match? '("Class"
                      ": "
                      (:value "clojure.lang.Atom" 0)
                      (:newline)
                      (:newline))
                    (header rendered))))
      (testing "renders the contains section"
        (is (match? (cond-> '("--- Contains:"
                              (:newline)
                              "  " "Class" ": " (:value "clojure.lang.PersistentArrayMap" 1)
                              (:newline)
                              (:newline)
                              "  --- Contents:"
                              (:newline)
                              "    " (:value ":a" 2) " = " (:value "1" 3)
                              (:newline))
                      datafy? (concat ['(:newline)]))
                    (section "Contains" rendered))))
      (when datafy?
        (testing "renders the datafy section"
          (is (match? '("--- Datafy:"
                        (:newline)
                        "  " "0" ". " (:value "{ :a 1 }" 4)
                        (:newline))
                      (datafy-section rendered))))))))

(deftest inspect-atom-infinite-seq-test
  (testing "inspecting an atom holding an infinite seq"
    (let [rendered (-> (atom (repeat 1)) inspect (inspect/set-page-size 3) render)]
      (testing "renders the header section"
        (is (match? '("Class"
                      ": "
                      (:value "clojure.lang.Atom" 0)
                      (:newline)
                      (:newline))
                    (header rendered))))
      (testing "renders the contains section"
        (is (match? (cond-> '("--- Contains:"
                              (:newline)
                              "  " "Class" ": " (:value "clojure.lang.Repeat" 1)
                              (:newline)
                              (:newline)
                              "  --- Contents:"
                              (:newline)
                              "    " "0" ". " (:value "1" 2)
                              (:newline)
                              "    " "1" ". " (:value "1" 3)
                              (:newline)
                              "    " "2" ". " (:value "1" 4)
                              (:newline)
                              "    " "..."
                              (:newline))
                      datafy? (concat ['(:newline)]))
                    (section "Contains" rendered))))
      (when datafy?
        (testing "renders the datafy section"
          (is (match? '("--- Datafy:"
                        (:newline)
                        "  " "0" ". " (:value "( 1 1 1 1 1 ... )" 5)
                        (:newline))
                      (datafy-section rendered))))))))

(deftest inspect-clojure-string-namespace-test
  (testing "inspecting the clojure.string namespace"
    (let [result (-> (find-ns 'clojure.string) inspect render)]
      (testing "renders the header"
        (is (match? `("Class"
                      ": "
                      (:value "clojure.lang.Namespace" 0)
                      (:newline)
                      "Count"
                      ": "
                      (:value ~(case (:minor *clojure-version*)
                                 8 "748"
                                 9 "778"
                                 10 "786"
                                 "799")
                              1)
                      (:newline)
                      (:newline))
                    (header result))))
      (testing "renders the refer from section"
        (is (match? `("--- Refer from:"
                      (:newline)
                      "  "
                      (:value "clojure.core" 2)
                      " = "
                      (:value ~(str "[ #'clojure.core/primitives-classnames #'clojure.core/+' #'clojure.core/decimal? "
                                    "#'clojure.core/restart-agent #'clojure.core/sort-by ... ]") 3)
                      (:newline)
                      (:newline))
                    (section "Refer from" result))))
      (testing "renders the imports section"
        (is (match? `("--- Imports:"
                      (:newline)
                      "  " (:value ~(str "{ Enum java.lang.Enum, "
                                         "InternalError java.lang.InternalError, "
                                         "NullPointerException java.lang.NullPointerException, "
                                         "InheritableThreadLocal java.lang.InheritableThreadLocal, "
                                         "Class java.lang.Class, ... }") 4)
                      (:newline)
                      (:newline))
                    (section "Imports" result))))
      (testing "renders the interns section"
        (is (match? (cond-> `("--- Interns:"
                              (:newline)
                              "  " (:value ~(str "{ ends-with? #'clojure.string/ends-with?, "
                                                 "replace-first-char #'clojure.string/replace-first-char, "
                                                 "capitalize #'clojure.string/capitalize, "
                                                 "reverse #'clojure.string/reverse, join #'clojure.string/join, ... }") 5)
                              (:newline))
                      datafy? (concat ['(:newline)]))
                    (section "Interns" result))))
      (when datafy?
        (testing "renders the datafy from section"
          (is (match? `("--- Datafy:"
                        (:newline)
                        "  " (:value ":name" 6) " = " (:value "clojure.string" 7)
                        (:newline)
                        "  " (:value ":publics" 8) " = "
                        (:value ~(str "{ blank? #'clojure.string/blank?, capitalize "
                                      "#'clojure.string/capitalize, ends-with? #'clojure.string/ends-with?, "
                                      "escape #'clojure.string/escape, includes? #'clojure.string/includes?, ... }") 9)
                        (:newline)
                        "  " (:value ":imports" 10) " = "
                        (:value ~(str "{ AbstractMethodError java.lang.AbstractMethodError, Appendable java.lang.Appendable, "
                                      "ArithmeticException java.lang.ArithmeticException, ArrayIndexOutOfBoundsException "
                                      "java.lang.ArrayIndexOutOfBoundsException, ArrayStoreException java.lang.ArrayStoreException, ... }") 11)
                        (:newline)
                        "  " (:value ":interns" 12) " = "
                        (:value ~(str "{ blank? #'clojure.string/blank?, capitalize #'clojure.string/capitalize, ends-with? #'clojure.string/ends-with?, "
                                      "escape #'clojure.string/escape, includes? #'clojure.string/includes?, ... }") 13)
                        (:newline))
                      (datafy-section result))))))))

(deftest inspect-datafiable-metadata-extension-test
  (testing "inspecting a map extended with the Datafiable protocol"
    (let [rendered (-> (extend-datafy-class {:name "John Doe"}) inspect render)]
      (testing "renders the header"
        (is (match? '("Class"
                      ": "
                      (:value "clojure.lang.PersistentArrayMap" 0)
                      (:newline)
                      (:newline))
                    (header rendered))))
      (testing "renders the meta information section"
        (is (match? '("--- Meta Information:"
                      (:newline)
                      "  "
                      (:value "clojure.core.protocols/datafy" 1)
                      " = "
                      (:value "orchard.inspect_test$extend_datafy_class$fn" 2)
                      (:newline)
                      (:newline))
                    (demunge (section "Meta Information" rendered)))))
      (when datafy?
        (testing "renders the datafy section"
          (is (match? '("--- Datafy:"
                        (:newline)
                        "  " (:value ":name" 5) " = " (:value "\"John Doe\"" 6)
                        (:newline)
                        "  " (:value ":class" 7) " = " (:value "\"PersistentArrayMap\"" 8)
                        (:newline))
                      (datafy-section rendered))))))))

(deftest inspect-navigable-metadata-extension-test
  (testing "inspecting a map extended with the Navigable protocol"
    (let [rendered (-> (extend-nav-vector {:name "John Doe"}) inspect render)]
      (testing "renders the header"
        (is (match? '("Class"
                      ": "
                      (:value "clojure.lang.PersistentArrayMap" 0)
                      (:newline)
                      (:newline))
                    (header rendered))))
      (testing "renders the meta information section"
        (is (match? '("--- Meta Information:"
                      (:newline)
                      "  " (:value "clojure.core.protocols/nav" 1)
                      " = " (:value "orchard.inspect_test$extend_nav_vector$fn" 2)
                      (:newline)
                      (:newline))
                    (demunge (section "Meta Information" rendered)))))
      (when datafy?
        (testing "renders the datafy section"
          (is (match? '("--- Datafy:"
                        (:newline)
                        "  " (:value ":name" 5) " = " (:value "[ :name \"John Doe\" ]" 6)
                        (:newline))
                      (datafy-section rendered))))))))

(deftest inspect-throwable-test
  (testing "inspecting a throwable"
    (let [rendered (-> (doto ^Throwable (ex-info "BOOM" {})
                         (.setStackTrace (into-array StackTraceElement [])))
                       inspect render)]
      (testing "renders the header"
        (is (match? `("Class"
                      ": " (:value "clojure.lang.ExceptionInfo" 0)
                      (:newline)
                      "Value" ": "
                      (:value
                       ~(if (= 8 (:minor *clojure-version*))
                          (str (str "\"#error {\\n :cause \\\"BOOM\\\"\\n :data {}\\n "
                                    ":via\\n [{:type clojure.lang.ExceptionInfo\\n   "
                                    ":message \\\"BOOM\\\"\\n   "
                                    ":data {}\\n   :at nil}]\\n :trace\\n []}\""))
                          (str "\"#error {\\n :cause \\\"BOOM\\\"\\n :data {}\\n :via\\n "
                               "[{:type clojure.lang.ExceptionInfo\\n   "
                               ":message \\\"BOOM\\\"\\n   :data {}}]\\n :trace\\n []}\"")) 1)
                      (:newline)
                      (:newline))
                    (header rendered))))
      (when datafy?
        (testing "renders the datafy section"
          (is (match? (case java-api-version
                        (11 16 17)
                        '("--- Datafy:"
                          (:newline)
                          "  " (:value ":via" 34) " = " (:value "[ { :type clojure.lang.ExceptionInfo, :message \"BOOM\", :data {} } ]" 35)
                          (:newline)
                          "  " (:value ":trace" 36) " = " (:value "[]" 37)
                          (:newline)
                          "  " (:value ":cause" 38) " = " (:value "\"BOOM\"" 39)
                          (:newline)
                          "  " (:value ":data" 40) " = " (:value "{}" 41)
                          (:newline))
                        '("--- Datafy:"
                          (:newline)
                          "  " (:value ":via" 30) " = " (:value "[ { :type clojure.lang.ExceptionInfo, :message \"BOOM\", :data {} } ]" 31)
                          (:newline)
                          "  " (:value ":trace" 32) " = " (:value "[]" 33)
                          (:newline)
                          "  " (:value ":cause" 34) " = " (:value "\"BOOM\"" 35)
                          (:newline)
                          "  " (:value ":data" 36) " = " (:value "{}" 37)
                          (:newline)))
                      (datafy-section rendered))))))))
