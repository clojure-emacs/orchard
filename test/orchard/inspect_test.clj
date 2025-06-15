(ns orchard.inspect-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [clojure.walk :as walk]
   [matcher-combinators.matchers :as matchers]
   [orchard.inspect :as inspect]
   [orchard.misc :as misc :refer [java-api-version]]
   [orchard.test.util :refer [is+]])
  (:import
   (orchard.java PrivateFieldClass)))

(defn- demunge-str [s]
  (-> s
      (str/replace #"(?i)\$([a-z-]+)__([0-9]+)(@[a-f0-9]+)?" "\\$$1")
      (str/replace #"(?i)(fn|eval)--([0-9]+)" "$1")))

(defn- demunge
  ([rendered]
   (demunge rendered demunge-str))
  ([rendered demunge-fn]
   (walk/prewalk (fn [form]
                   (if (string? form)
                     (demunge-fn form)
                     form))
                 rendered)))

;; Simplifies writing test structures for `match?`.

(def nil-result
  ["Value: nil" [:newline] [:newline] "--- Contents:" [:newline] string? [:newline]])

(def code "(sorted-map :a {:b 1} :c \"a\" :d 'e :f [2 3])")

(def eval-result (eval (read-string code)))

(def inspect-result
  ["Class: "
   [:value "clojure.lang.PersistentTreeMap" 0]
   [:newline]
   "Count: 4"
   [:newline]
   [:newline]
   "--- Contents:"
   [:newline]
   "  " [:value ":a" 1] " = " [:value "{:b 1}" 2]
   [:newline]
   "  " [:value ":c" 3] " = " [:value "\"a\"" 4]
   [:newline]
   "  " [:value ":d" 5] " = " [:value "e" 6]
   [:newline]
   "  " [:value ":f" 7] " = " [:value "[2 3]" 8]
   [:newline] [:newline]
   #"--- View mode" [:newline] "  ●normal object pretty"])

(def long-sequence (range 70))
(def long-vector (vec (range 70)))
(def long-map (into (sorted-map) (zipmap (range 70) (range 70))))
(def long-nested-coll (vec (map #(range (* % 10) (+ (* % 10) 80)) (range 200))))

(defrecord TestRecord [a b c d])

(defn- section? [name rendered]
  (when (string? rendered)
    (re-find (re-pattern (str "^--- " name)) rendered)))

(defn- section [rendered name]
  (->> rendered
       (drop-while #(not (section? name %)))
       (take-while #(or (section? name %)
                        (not (section? ".*" %))))
       ;; Trim newlines
       reverse
       (drop-while #(= % [:newline]))
       reverse
       (not-empty)))

(defn- datafy-section [rendered]
  (section rendered "Datafy"))

(defn- contents-section [rendered]
  (section rendered "Contents"))

(defn- header [rendered]
  (take-while #(not (and (string? %)
                         (re-matches #".*---.*" %))) rendered))

(defn- labeled-value [label rendered]
  (let [formatted-label (str label ": ")]
    (->> rendered
         (drop-while #(not (= formatted-label %)))
         (take 2))))

(defn- page-size-info [rendered]
  (when-let [sec (section rendered "Page Info")]
    (last sec)))

(defn- extend-datafy-class [m]
  (vary-meta m assoc 'clojure.core.protocols/datafy (fn [x] (assoc x :class (.getSimpleName (class x))))))

(defn- extend-nav-vector [m]
  (vary-meta m assoc 'clojure.core.protocols/nav (fn [coll k v] [k (get coll k v)])))

(def inspect inspect/start)

(defn render
  [inspector]
  (:rendered inspector))

(defn set-page-size [inspector new-size]
  (inspect/refresh inspector {:page-size new-size}))

(defn set-pretty-print [inspector pretty-print]
  (inspect/refresh inspector {:pretty-print pretty-print}))

(deftest nil-test
  (testing "nil renders correctly"
    (is+ nil-result
         (-> nil
             inspect
             render))))

(deftest pop-empty-test
  (testing "popping an empty inspector renders nil"
    (is+ nil-result
         (-> (inspect nil)
             inspect/up
             render))))

(deftest pop-empty-idempotent-test
  (testing "popping an empty inspector is idempotent"
    (is+ nil-result
         (-> (inspect nil)
             inspect/up
             inspect/up
             render))))

(deftest push-empty-test
  (testing "pushing an empty inspector index renders nil"
    (is+ nil-result
         (-> (inspect nil)
             (inspect/down 1)
             render))))

(deftest push-empty-idempotent-test
  (testing "pushing an empty inspector index is idempotent"
    (is+ nil-result
         (-> (inspect nil)
             (inspect/down 1)
             (inspect/down 1)
             render))))

(def any-var 42)

(deftest inspect-var-test
  (testing "inspecting a var"
    (let [rendered (-> #'any-var inspect render)]
      (testing "renders the header"
        (is+ ["Class: "
              [:value "clojure.lang.Var" 0]
              [:newline]
              "Value: "
              [:value "42" 1]
              [:newline]
              [:newline]]
             (header rendered)))
      (testing "renders the meta information section"
        (is+ ["--- Meta Information:"
              [:newline]
              "  " [:value ":line" pos?] " = " [:value string? pos?]
              [:newline]
              "  " [:value ":column" pos?] " = " [:value string? pos?]
              [:newline]
              "  " [:value ":file" pos?] " = " [:value string? pos?]
              [:newline]
              "  " [:value ":name" pos?] " = " [:value "any-var" pos?]
              [:newline]
              "  " [:value ":ns" pos?] " = " [:value "#namespace[orchard.inspect-test]" pos?]]
             (section rendered "Meta Information")))
      (testing "renders the datafy section"
        (is+ ["--- Datafy:"
              [:newline]
              "  0. " [:value "42" pos?]]
             (datafy-section rendered))))))

(deftest inspect-expr-test
  (testing "rendering an expr"
    (is+ inspect-result
         (-> eval-result
             inspect
             render))))

(deftest push-test
  (testing "pushing a rendered expr inspector idx"
    (is+ (matchers/prefix
          ["Class: "
           [:value "clojure.lang.PersistentArrayMap" number?]
           [:newline]
           "Count: 1"
           [:newline]
           [:newline]
           "--- Contents:"
           [:newline]
           "  " [:value ":b" number?] " = " [:value "1" number?]
           [:newline]
           [:newline]
           "--- Path:"
           [:newline]
           "  :a"])
         (-> eval-result inspect (inspect/down 2) render))))

(deftest pop-test
  (testing "popping a rendered expr inspector"
    (is+ inspect-result
         (-> eval-result
             inspect
             (inspect/down 2)
             inspect/up
             render))))

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
            page-size-info)))
  (testing "small collections are not paginated"
    (is (nil? (-> (range 10)
                  inspect
                  :rendered
                  page-size-info))))
  (testing "changing page size"
    (is (= 21 (-> long-sequence
                  inspect
                  (set-page-size 20)
                  :counter)))
    (is (= 41 (-> long-map
                  inspect
                  (set-page-size 20)
                  :counter)))
    (is (nil? (-> long-sequence
                  inspect
                  (set-page-size 200)
                  :rendered
                  page-size-info))))
  (testing "uncounted collections have their size determined on the last page"
    (is (= "  Page size: 32, showing page: 2 of 2"
           (-> (range 50)
               inspect
               inspect/next-page
               :rendered
               page-size-info))))
  (testing "next-page and prev-page are bound to collection size"
    (is (= 0
           (-> []
               inspect
               inspect/next-page
               inspect/next-page
               inspect/next-page
               inspect/next-page
               inspect/next-page
               inspect/next-page
               :current-page)))
    (is (= (-> []
               inspect)
           (-> []
               inspect
               inspect/next-page
               inspect/next-page
               inspect/next-page
               inspect/next-page
               inspect/next-page
               inspect/next-page)))
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
    (is (= (-> []
               inspect)
           (-> []
               inspect
               inspect/prev-page
               inspect/prev-page
               inspect/prev-page)))
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
        (is (= 4 (:current-page (inspect/up ins)))))))
  (testing "pagination commands are no-ops on non-pageable objects"
    (let [ins (inspect 42)]
      (is+ (render ins)
           (render (-> ins
                       inspect/next-page
                       inspect/next-page
                       inspect/prev-page
                       inspect/prev-page))))
    ;; Atom deref contents are non-pageable.
    (let [ins (inspect (atom (range 100)))]
      (is+ (render ins)
           (render (-> ins
                       inspect/next-page
                       inspect/next-page
                       inspect/prev-page
                       inspect/prev-page))))))

(deftest def-value-test
  (testing "define var with the currently inspected value"
    (-> eval-result
        inspect
        (inspect/down 2)
        (inspect/down 2)
        (inspect/def-current-value *ns* "--test-val--"))
    (is (= 1 @(resolve '--test-val--)))))

(deftest down-test
  (testing "basic down"
    (is (= 2 (-> (list 1 2)
                 inspect
                 (inspect/down 2)
                 :value)))
    (is (= 2 (-> [1 2]
                 inspect
                 (inspect/down 2)
                 :value)))
    (is (= 1 (-> {:a 1 :b 2}
                 inspect
                 (inspect/down 2)
                 :value)))
    (is (= 2 (-> #{1 2}
                 inspect
                 (inspect/down 2)
                 :value)))
    (is (= :a (-> '{:foo [:a :b :c]}
                  inspect
                  (inspect/down 2)
                  (inspect/down 1)
                  :value)))
    (is (= 19 (-> long-sequence
                  inspect
                  (inspect/down 20)
                  :value)))
    (is (= 9 (-> long-map
                 inspect
                 (inspect/down 20)
                 :value))))
  (testing "down with pagination"
    (is (= long-sequence (-> long-sequence
                             inspect
                             (set-page-size 2)
                             (inspect/down 20)
                             :value)))
    (is (= long-map (-> long-map
                        inspect
                        (set-page-size 2)
                        (inspect/down 20)
                        :value)))
    (is (= 19 (-> long-map
                  inspect
                  (inspect/down 40)
                  :value))))
  (testing "doesn't go out of boundaries"
    (is (= [1 2] (-> [1 2]
                     inspect
                     (inspect/down 10)
                     :value)))
    (is (= [1 2] (-> [1 2]
                     inspect
                     (set-page-size 1)
                     (inspect/down 10)
                     :value)))
    (is (= [1 2] (-> [1 2]
                     inspect
                     (inspect/down 5)
                     (inspect/down -10)
                     :value)))
    (is (= 2 (-> [1 2]
                 inspect
                 (inspect/down 1)
                 (inspect/next-sibling)
                 (inspect/next-sibling)
                 :value)))
    (is (= 2 (-> {:a 1 :b 2}
                 inspect
                 (inspect/down 4)
                 (inspect/next-sibling)
                 (inspect/next-sibling)
                 :value)))
    (is (= 1 (-> {:a 1 :b 2}
                 inspect
                 (set-page-size 1)
                 (inspect/down 2)
                 (inspect/next-sibling)
                 (inspect/next-sibling)
                 (inspect/next-sibling)
                 :value)))
    (is (= 1 (-> [1 2]
                 inspect
                 (inspect/down 1)
                 (inspect/previous-sibling)
                 (inspect/previous-sibling)
                 (inspect/previous-sibling)
                 :value)))))

(deftest sibling*-test
  (is (= :c
         (-> '{:foo [:a :b :c]}
             inspect
             (inspect/down 2)
             (inspect/down 1)
             (inspect/next-sibling)
             (inspect/next-sibling)
             :value)))
  (is (= :b
         (-> '{:foo [:a :b :c]}
             inspect
             (inspect/down 2)
             (inspect/down 1)
             (inspect/next-sibling)
             (inspect/next-sibling)
             (inspect/previous-sibling)
             :value)))
  (is (= :a
         (-> '{:foo [:a :b :c]}
             inspect
             (inspect/down 2)
             (inspect/down 1)
             (inspect/previous-sibling)
             (inspect/previous-sibling)
             (inspect/previous-sibling)
             (inspect/previous-sibling)
             :value)))
  (is (= :c
         (-> '{:foo [:a :b :c]}
             inspect
             (inspect/down 2)
             (inspect/down 1)
             (inspect/next-sibling)
             (inspect/next-sibling)
             (inspect/next-sibling)
             (inspect/next-sibling)
             (inspect/next-sibling)
             (inspect/next-sibling)
             (inspect/next-sibling)
             :value)))
  (let [inspector-at-first-sibling (-> '{:foo [:a :b :c]}
                                       inspect
                                       (inspect/down 2)
                                       (inspect/down 1))]
    (is (= inspector-at-first-sibling
           (-> inspector-at-first-sibling
               inspect/previous-sibling
               inspect/previous-sibling
               inspect/previous-sibling
               inspect/previous-sibling))))
  (let [inspector-at-last-sibling (-> '{:foo [:a :b :c]}
                                      inspect
                                      (inspect/down 2)
                                      (inspect/down 1)
                                      (inspect/next-sibling)
                                      (inspect/next-sibling))]
    (is (= inspector-at-last-sibling
           (-> inspector-at-last-sibling
               inspect/next-sibling
               inspect/next-sibling
               inspect/next-sibling
               inspect/next-sibling))))
  (testing "next and previous siblings with pagination"
    (is (= {:value 32 :pages-stack [1]}
           (-> long-vector
               inspect
               (inspect/down 32)
               (inspect/next-sibling)
               (select-keys [:value :pages-stack]))))
    (is (= {:value 31 :pages-stack [0]}
           (-> long-vector
               inspect
               (inspect/next-page)
               (inspect/down 1)
               (inspect/previous-sibling)
               (select-keys [:value :pages-stack]))))
    (is (= 3
           (-> long-vector
               inspect
               (set-page-size 1)
               (inspect/down 1)
               (inspect/next-sibling)
               (inspect/next-sibling)
               (inspect/next-sibling)
               (inspect/up)
               :current-page)))
    (is (= 28 (-> long-vector
                  inspect
                  (inspect/down 32)
                  (inspect/previous-sibling)
                  (inspect/previous-sibling)
                  (inspect/previous-sibling)
                  :value)))
    (is (= 32 (-> long-vector
                  inspect
                  (inspect/down 32)
                  (inspect/next-sibling)
                  :value))))
  (testing "next-sibling doesn't fall beyond the last element."
    (is (= 3 (-> [1 2 3]
                 inspect
                 (inspect/down 2)
                 (inspect/next-sibling)
                 (inspect/next-sibling)
                 (inspect/next-sibling)
                 (inspect/next-sibling)
                 :value)))
    (is (= 69 (-> long-sequence
                  inspect
                  (inspect/next-page)
                  (inspect/next-page)
                  (inspect/down 4)
                  (inspect/next-sibling)
                  (inspect/next-sibling)
                  (inspect/next-sibling)
                  (inspect/next-sibling)
                  (inspect/next-sibling)
                  (inspect/next-sibling)
                  (inspect/next-sibling)
                  :value))))
  (testing "sibling functions work with arrays"
    (is+ {:value 35, :pages-stack [1], :path '[(nth 35)]}
         (-> (byte-array (range 40))
             inspect
             (inspect/down 33)
             (inspect/next-sibling)
             (inspect/next-sibling)
             (inspect/next-sibling)
             (inspect/next-sibling)))))

(deftest path-test
  (let [t {:a (list 1 2 {:b {:c (vec (map (fn [x] {:foo (* x 10)}) (range 100)))}})
           :z 42}
        inspector (-> (inspect t)
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
                      (inspect/down 10))]
    (is+ ["--- Path:" [:newline] "  :a (nth 2) :b :c (nth 73)"] (-> inspector render (section "Path"))))
  (testing "inspector tracks the path in the data structure beyond the first page with custom page size"
    (is+ ["--- Path:" [:newline] "  (get 2)"]
         (-> long-map inspect
             (set-page-size 2)
             (inspect/next-page)
             (inspect/down 2)
             render
             (section "Path"))))
  (testing "doesn't show path if unknown navigation has happened"
    (is+ nil (-> long-map inspect (inspect/down 39) render (section "Path")))
    (is+ nil (-> long-map inspect (inspect/down 40) (inspect/down 0) render (section "Path")))
    (is+ nil (-> long-map inspect (inspect/down 40) (inspect/down 0) (inspect/down 1) render (section "Path"))))
  (testing "doesn't show the path in the top level"
    (is+ nil (-> [1 2 3] inspect render (section "Path")))))

(deftest inspect-class-fields-test
  (testing "inspecting a class with fields renders correctly"
    (is+ ["--- Fields:"
          [:newline]
          "  " [:value "public static final Boolean FALSE" pos?]
          [:newline]
          "  " [:value "public static final Boolean TRUE" pos?]
          [:newline]
          "  " [:value "public static final Class<Boolean> TYPE" pos?]]
         (-> Boolean inspect render (section "Fields"))))
  (testing "inspecting a class without fields renders correctly"
    (is (nil? (-> Object inspect render (section "Fields"))))))

(deftest inspect-coll-test
  (testing "inspect :coll prints contents of the coll"
    (is+ (matchers/prefix
          ["Class: "
           [:value "clojure.lang.PersistentVector" number?]
           [:newline]
           "Count: 4"
           [:newline]
           [:newline]
           "--- Contents:"
           [:newline]
           "  0. " [:value "1" number?]
           [:newline]
           "  1. " [:value "2" number?]
           [:newline]
           "  2. " [:value "nil" number?]
           [:newline]
           "  3. " [:value "3" number?]
           [:newline]])
         (render (inspect [1 2 nil 3]))))

  (testing "inspect :coll aligns index numbers so that values appear aligned"
    (is+ (matchers/prefix
          ["Class: "
           [:value "clojure.lang.PersistentVector" number?]
           [:newline]
           "Count: 11"
           [:newline]
           [:newline]
           "--- Contents:"
           [:newline]
           "   0. " [:value "0" number?]
           [:newline]
           "   1. " [:value "1" number?]
           [:newline]
           "   2. " [:value "2" number?]
           [:newline]
           "   3. " [:value "3" number?]
           [:newline]
           "   4. " [:value "4" number?]
           [:newline]
           "   5. " [:value "5" number?]
           [:newline]
           "   6. " [:value "6" number?]
           [:newline]
           "   7. " [:value "7" number?]
           [:newline]
           "   8. " [:value "8" number?]
           [:newline]
           "   9. " [:value "9" number?]
           ;; Numbers above have padding, "10" below doesn't.
           [:newline]
           "  10. " [:value "10" number?]
           [:newline]])
         (render (inspect (vec (range 11))))))

  (testing "inspect :coll aligns index numbers correctly for page size > 100"
    (let [rendered (-> (inspect (vec (range 101)))
                       (set-page-size 200)
                       render)
          tail (take-last 2 (contents-section rendered))]
      (is+ (matchers/prefix
            ["Class: "
             [:value "clojure.lang.PersistentVector" number?]
             [:newline]
             "Count: 101"
             [:newline]
             [:newline]
             "--- Contents:"
             [:newline]
             "    0. " [:value "0" number?]])
           rendered)
      ;; "  0" has two spaces of padding, "100" below has none.
      (is+ ["  100. " [:value "100" pos?]] tail))))

(deftest inspect-coll-meta-test
  (testing "inspecting a collection with metadata renders the metadata section"
    (testing "renders the meta information section"
      (let [rendered (render (inspect (with-meta [:a :b :c :d :e] {:m 42})))]
        (is+ ["--- Meta Information:"
              [:newline]
              "  "
              [:value ":m" 1]
              " = "
              [:value "42" 2]]
             (section rendered "Meta Information"))))

    (testing "meta values can be navigated to"
      (is (= 42 (-> (inspect (with-meta [:a :b :c :d :e] {:m 42}))
                    (inspect/down 2)
                    :value))))

    (testing "regular values can be navigated to"
      (is (= :a (-> (inspect (with-meta [:a :b :c :d :e] {:m 42}))
                    (inspect/down 3)
                    :value)))
      (is (= :e (-> (inspect (with-meta [:a :b :c :d :e] {:m 42}))
                    (inspect/down 7)
                    :value))))

    (testing "if meta is larger than page size, render it as a single value"
      (let [rendered (-> [:a :b :c :d :e]
                         (with-meta (zipmap (range 20) (range)))
                         inspect
                         (set-page-size 10)
                         render)]
        (is+ ["--- Meta Information:"
              [:newline]
              "  "
              [:value "{0 0, 7 7, 1 1, 4 4, 15 15, ...}" pos?]]
             (section rendered "Meta Information"))))))

(deftest inspect-coll-nav-test
  (testing "inspecting a collection extended with the Datafiable and Navigable protocols"
    (let [ins (-> (->> (iterate inc 0)
                       (map #(hash-map :x %))
                       (map extend-datafy-class)
                       (map extend-nav-vector))
                  inspect (set-page-size 2))
          rendered (render ins)]
      (testing "renders the content section"
        (is+ ["--- Contents:"
              [:newline]
              "  0. " [:value "{:x 0}" pos?]
              [:newline]
              "  1. " [:value "{:x 1}" pos?]
              [:newline]
              "  ..."]
             (contents-section rendered)))
      (testing "renders the datafy section"
        (is+ ["--- Datafy:"
              [:newline]
              "  0. " [:value "{:class \"PersistentHashMap\", :x 0}" pos?]
              [:newline]
              "  1. " [:value "{:class \"PersistentHashMap\", :x 1}" pos?]
              [:newline]
              "  ..."]
             (datafy-section rendered)))
      (testing "renders the page info section"
        (is+ ["--- Page Info:"
              [:newline]
              "  Page size: 2, showing page: 1 of ?"]
             (section rendered "Page Info")))
      (testing "follows the same pagination rules"
        (is+ ["--- Datafy:"
              [:newline]
              "  ..."
              [:newline]
              "  4. " [:value "{:class \"PersistentHashMap\", :x 4}" pos?]
              [:newline]
              "  5. " [:value "{:class \"PersistentHashMap\", :x 5}" pos?]
              [:newline]
              "  ..."]
             (-> ins
                 (inspect/next-page)
                 (inspect/next-page)
                 render
                 datafy-section))))))

(deftest inspect-configure-length-test
  (testing "inspect respects :max-atom-length and :max-coll-size configuration"
    (is+ (matchers/prefix
          ["Class: "
           [:value "clojure.lang.Persist..." 0]
           [:newline]
           "Count: 1"
           [:newline]
           [:newline]
           "--- Contents:"
           [:newline]
           "  0. " [:value "[111111 2222 333 ...]" 1]
           [:newline]])
         (-> (inspect/start {:max-atom-length 20
                             :max-coll-size 3}
                            [[111111 2222 333 44 5]])
             render)))
  (testing "inspect respects :max-value-length configuration"
    (is+ (matchers/prefix
          ["Class: "
           [:value "clojure.lang.PersistentVector" 0]
           [:newline]
           "Count: 1"
           [:newline]
           [:newline]
           "--- Contents:"
           [:newline]
           "  0. " [:value "(\"long value\" \"long value\" \"long value\" \"long valu..." 1]
           [:newline]])
         (-> (inspect/start {:max-value-length 50} [(repeat "long value")])
             render)))

  (testing "inspect respects :max-value-depth configuration"
    (is+ (matchers/prefix
          ["Class: "
           [:value "clojure.lang.PersistentVector" 0]
           [:newline]
           "Count: 1"
           [:newline]
           [:newline]
           "--- Contents:"
           [:newline]
           "  0. " [:value "[[[[[[...]]]]]]" 1]
           [:newline]])
         (-> (inspect/start {:max-nested-depth 5} [[[[[[[[[[1]]]]]]]]]])
             render))))

(deftest inspect-java-hashmap-test
  (testing "inspecting java.util.Map descendants prints a key-value coll"
    (let [^java.util.Map the-map {:a 1, :b 2, :c 3}
          rendered (render (inspect (java.util.HashMap. the-map)))
          contents (contents-section rendered)]
      (is+ (matchers/prefix ["Class: "
                             [:value "java.util.HashMap" 0]
                             [:newline]
                             "Count: 3"
                             [:newline]
                             [:newline]])
           rendered)
      (is+ (matchers/embeds ["  " [:value ":a" pos?] " = " [:value "1" pos?] [:newline]])
           contents)
      (is+ (matchers/embeds ["  " [:value ":b" pos?] " = " [:value "2" pos?] [:newline]])
           contents)
      (is+ (matchers/embeds ["  " [:value ":c" pos?] " = " [:value "3" pos?] [:newline]])
           contents))))

(deftest inspect-java-object-test
  (testing "inspecting any Java object prints its fields"
    (is+ (matchers/prefix
          ["Class: "
           [:value "clojure.lang.TaggedLiteral" 0]
           [:newline]
           "Value: " [:value "#foo ()" 1]
           [:newline]
           #"Identity hash code: "
           [:newline]
           [:newline]
           "--- Instance fields:"
           [:newline] "  " [:value "form" 2] " = " [:value "()" 3]
           [:newline] "  " [:value "tag" 4] " = " [:value "foo" 5]
           [:newline]
           [:newline]
           "--- Static fields:"
           [:newline] "  " [:value "FORM_KW" 6] " = " [:value ":form" 7]
           [:newline] "  " [:value "TAG_KW" 8] " = " [:value ":tag" 9]
           [:newline]])
         (render (inspect (clojure.lang.TaggedLiteral/create 'foo ()))))))

(deftest inspect-path
  (testing "basic paths"
    (is (= []
           (-> [1 2]
               inspect
               :path)))
    (is (= '[(nth 1)]
           (-> [1 2]
               inspect
               (set-page-size 1)
               (inspect/next-page)
               (inspect/next-page)
               (inspect/next-page)
               (inspect/down 1)
               (inspect/next-sibling)
               :path)))
    (is (= '[:b]
           (-> {:a 1 :b 2}
               inspect
               (set-page-size 1)
               (inspect/next-page)
               (inspect/next-page)
               (inspect/next-page)
               (inspect/down 2)
               :path)))
    (is (= '[<unknown>]
           (-> {:a 1 :b 2}
               inspect
               (set-page-size 1)
               (inspect/next-page)
               (inspect/next-page)
               (inspect/next-page)
               (inspect/down 1)
               :path))))
  (testing "inspector keeps track of the path in the inspected structure"
    (let [t {:a (list 1 2 {:b {:c (vec (map (fn [x] {:foo (* x 10)}) (range 100)))}})
             :z 42}
          inspector (-> (inspect t)
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
                        (inspect/down 10))]
      (is (= '[:a (nth 2) :b :c (nth 73)] (:path inspector)))
      (is (= '[:a (nth 2) :b :c (nth 73) <unknown>]
             (:path (-> inspector (inspect/down 0)))))
      (is (= '[:a (nth 2) :b :c]
             (:path (-> inspector (inspect/down 0) (inspect/down 0)
                        (inspect/up) (inspect/up) (inspect/up)))))))
  (testing "path tracking works if object has metadata"
    (is (= [:data]
           (-> (inspect (with-meta {:data 1} {:m 2}))
               (inspect/down 4)
               :path)))))

(defn- rendered-hier [indents-and-classnames]
  (mapcat (fn [[indent class]]
            [indent [:value class number?] [:newline]])
          (partition 2 indents-and-classnames)))

(deftest inspect-class-test
  (testing "inspecting the java.lang.Object class"
    (let [rendered (-> Object inspect render)]
      (testing "renders the header section"
        (is+ ["Name: "
              [:value "java.lang.Object" 0] [:newline]
              "Class: " [:value "java.lang.Class" 1] [:newline] [:newline]]
             (header rendered)))
      (testing "renders the constructors section"
        (is+ ["--- Constructors:"
              [:newline]
              "  " [:value "public Object()" 2]]
             (section rendered "Constructors")))
      (testing "renders the methods section"
        (let [methods (section rendered "Methods")]
          (is+ (matchers/embeds [[:value "public final native Class<?> getClass()" pos?]
                                 [:value "public boolean equals(Object)" pos?]
                                 [:value "public native int hashCode()" pos?]
                                 [:value "public final native void notify()" pos?]
                                 [:value "public final native void notifyAll()" pos?]
                                 [:value "public String toString()" pos?]
                                 [:value "public final void wait() throws InterruptedException" pos?]
                                 [:value "public final void wait(long,int) throws InterruptedException" pos?]])
               methods)))
      (testing "renders the datafy section"
        (is+ ["--- Datafy:"
              [:newline]
              "  " [:value ":flags" pos?] " = " [:value "#{:public}" pos?]
              [:newline]
              "  " [:value ":members" pos?] " = "
              [:value #=(str "{clone [#Method{:name clone, :return-type java.lang.Object, :declaring-class java.lang.Object, "
                             ":parameter-types [], :exception-types [java.lang.CloneNotSupportedException], ...}], equals "
                             "[#Method{:name equals, :return-type boolean, :declaring-class java.lang.Object, :parameter-types "
                             "[java.lang.Object], :exception-types [], ...}], finalize [#Method{:name finalize, :return-type void, "
                             ":declaring-class java.lang.Object, :parameter-types [], :exception-types [java.lang.Throwable], "
                             "...}], getClass [#Method{:name getClass, :return-type java.lang.Class, :declaring-class java.lang.Object, "
                             ":parameter-types [], :exception-types [], ...}], hashCode [#Method{:name hashCode, :return-type int, "
                             ":declaring-class java.lang.Object, :parameter-types [], :exception-types [], ...}], ...}")
               pos?]
              [:newline]
              "  " [:value ":name" pos?] " = " [:value "java.lang.Object" pos?]]
             (datafy-section rendered)))))

  (testing "inspecting the java.lang.Class class"
    (let [rendered (-> Class inspect render)]
      (testing "renders the class hierarchy section"
        (is+ (matchers/prefix ["--- Class hierarchy:"
                               [:newline]
                               "  " [:value "java.lang.Object" pos?]
                               [:newline]
                               "  " [:value "java.io.Serializable" pos?]
                               [:newline]])
             (section rendered "Class hierarchy")))))

  (testing "inspecting the java.io.FileReader  class"
    (let [rendered (-> java.io.FileReader inspect render)]
      (testing "renders the class hierarchy section"
        (is+ (butlast
              (concat ["--- Class hierarchy:" [:newline]]
                      (rendered-hier ["  " "java.io.InputStreamReader"
                                      "    " "java.io.Reader"
                                      "      " "java.lang.Object"
                                      "      " "java.io.Closeable"
                                      "        " "java.lang.AutoCloseable"
                                      "      " "java.lang.Readable"])))
             (section rendered "Class hierarchy")))))

  (testing "inspecting the java.lang.ClassValue class"
    (let [rendered (-> java.lang.ClassValue inspect render)]
      (testing "renders the header section"
        (is+ ["Name: "
              [:value "java.lang.ClassValue" 0] [:newline]
              "Class: " [:value "java.lang.Class" 1] [:newline] [:newline]]
             (header rendered)))
      (testing "renders the methods section"
        (let [methods (section rendered "Methods")]
          (is+ (matchers/prefix ["--- Methods:" [:newline]])
               methods)
          (doseq [assertion ["public boolean equals(Object)"
                             "public T get(Class<?>)"
                             "public final native Class<?> getClass()"
                             "public native int hashCode()"
                             "public final native void notify()"
                             "public final native void notifyAll()"
                             "public void remove(Class<?>)"
                             "public String toString()"
                             "public final void wait() throws InterruptedException"
                             "public final void wait(long,int) throws InterruptedException"]]
            (is+ (matchers/embeds ["  " [:value assertion pos?]])
                 methods))))))

  (testing "inspecting an internal class"
    (is+ ["--- Fields:"
          [:newline] "  "
          [:value "public volatile clojure.lang.MethodImplCache __methodImplCache" pos?]]
         (-> clojure.lang.AFunction$1 inspect render (section "Fields")))))

(deftest inspect-method-test
  (testing "reflect.Method values aren't truncated"
    (let [rendered (-> (.getDeclaredMethod clojure.lang.AFn "invoke"
                                           (into-array Class (repeat 15 Object)))
                       inspect render)]
      (is+ (matchers/embeds [[:value "public Object invoke(Object,Object,Object,Object,Object,Object,Object,Object,Object,Object,Object,Object,Object,Object,Object)" 1]])
           rendered))))

(deftest inspect-atom-test
  (testing "inspecting an atom"
    (let [rendered (-> (atom {:a 1}) inspect render)]
      (testing "renders the header section"
        (is+ ["Class: "
              [:value "clojure.lang.Atom" 0]
              [:newline]
              [:newline]]
             (header rendered)))
      (testing "renders the deref section"
        (is+ ["--- Deref:"
              [:newline]
              "  Class: " [:value "clojure.lang.PersistentArrayMap" 1]
              [:newline]
              "  Count: 1"
              [:newline]
              [:newline]
              "  --- Contents:"
              [:newline]
              "    " [:value ":a" 2] " = " [:value "1" 3]]
             (section rendered "Deref")))
      (testing "doesn't render the datafy section"
        (is+ nil (datafy-section rendered)))))

  (testing "small collection is rendered fully"
    (is+ ["--- Deref:"
          [:newline]
          "  Class: " [:value "clojure.lang.LongRange" 1]
          [:newline]
          "  Count: 3"
          [:newline]
          [:newline]
          "  --- Contents:"
          [:newline]
          "    0. " [:value "0" 2]
          [:newline]
          "    1. " [:value "1" 3]
          [:newline]
          "    2. " [:value "2" 4]]
         (-> (atom (range 3)) inspect render (section "Deref"))))

  (testing "larger collection is rendered as a single value"
    (is+ ["--- Deref:"
          [:newline]
          "  Class: " [:value "clojure.lang.LongRange" 1]
          [:newline]
          "  Count: 100" [:newline] [:newline]
          "  --- Contents:"
          [:newline]
          "    " [:value "(0 1 2 3 4 ...)" 2]]
         (-> (atom (range 100)) inspect render (section "Deref"))))

  (testing "meta is shown on atoms"
    (is+ ["--- Meta Information:"
          [:newline]
          "  " [:value ":foo" 1] " = " [:value "\"bar\"" 2]]
         (-> (atom [1 2 3] :meta {:foo "bar"}) inspect render (section "Meta Information")))))

(deftest inspect-atom-infinite-seq-test
  (testing "inspecting an atom holding an infinite seq"
    (let [rendered (-> (atom (repeat 1)) inspect (set-page-size 3) render)]
      (testing "renders the header section"
        (is+ ["Class: "
              [:value "clojure.lang.Atom" 0]
              [:newline]
              [:newline]]
             (header rendered)))
      (testing "renders the deref section"
        (is+ ["--- Deref:" [:newline]
              "  Class: " [:value "clojure.lang.Repeat" 1] [:newline]
              [:newline]
              "  --- Contents:" [:newline]
              "    " [:value "(1 1 1 1 1 ...)" 2]]
             (section rendered "Deref"))))))

(deftest inspect-clojure-string-namespace-test
  (testing "inspecting the clojure.string namespace"
    (let [result (-> (find-ns 'clojure.string) inspect render)]
      (testing "renders the header"
        (is+ ["Class: " [:value "clojure.lang.Namespace" number?] [:newline]
              #"^Count: " [:newline]
              [:newline]]
             (header result)))
      (testing "renders the meta section"
        (is+ ["--- Meta Information:"
              [:newline]
              "  " [:value ":doc" pos?]
              " = "
              [:value string? pos?]
              [:newline]
              "  " [:value ":author" pos?]
              " = "
              [:value string? pos?]]
             (section result "Meta Information")))
      (testing "renders the refer from section"
        (is+ ["--- Refer from:"
              [:newline]
              "  "
              [:value "#namespace[clojure.core]" pos?]
              " = "
              [:value #=(str "[#'clojure.core/primitives-classnames #'clojure.core/+' #'clojure.core/decimal? "
                             "#'clojure.core/restart-agent #'clojure.core/sort-by ...]") pos?]]
             (section result "Refer from")))
      (testing "renders the imports section"
        (is+ ["--- Imports:"
              [:newline]
              "  " [:value #=(str "{Enum java.lang.Enum, "
                                  "InternalError java.lang.InternalError, "
                                  "NullPointerException java.lang.NullPointerException, "
                                  "InheritableThreadLocal java.lang.InheritableThreadLocal, "
                                  "Class java.lang.Class, ...}") pos?]]
             (section result "Imports")))
      (testing "renders the interns section"
        (is+ ["--- Interns:"
              [:newline]
              "  " [:value #=(str "{ends-with? #'clojure.string/ends-with?, "
                                  "replace-first-char #'clojure.string/replace-first-char, "
                                  "capitalize #'clojure.string/capitalize, "
                                  "reverse #'clojure.string/reverse, join #'clojure.string/join, ...}") pos?]]
             (section result "Interns")))
      (testing "renders the datafy from section"
        (is+ ["--- Datafy:"
              [:newline]
              "  " [:value ":name" 9] " = " [:value "clojure.string" pos?]
              [:newline]
              "  " [:value ":publics" pos?] " = "
              [:value #=(str "{blank? #'clojure.string/blank?, capitalize "
                             "#'clojure.string/capitalize, ends-with? #'clojure.string/ends-with?, "
                             "escape #'clojure.string/escape, includes? #'clojure.string/includes?, ...}") pos?]
              [:newline]
              "  " [:value ":imports" pos?] " = "
              [:value #=(str "{AbstractMethodError java.lang.AbstractMethodError, Appendable java.lang.Appendable, "
                             "ArithmeticException java.lang.ArithmeticException, ArrayIndexOutOfBoundsException "
                             "java.lang.ArrayIndexOutOfBoundsException, ArrayStoreException java.lang.ArrayStoreException, ...}") pos?]
              [:newline]
              "  " [:value ":interns" pos?] " = "
              [:value #=(str "{blank? #'clojure.string/blank?, capitalize #'clojure.string/capitalize, ends-with? #'clojure.string/ends-with?, "
                             "escape #'clojure.string/escape, includes? #'clojure.string/includes?, ...}") pos?]]
             (datafy-section result))))))

(deftest inspect-datafiable-metadata-extension-test
  (testing "inspecting a map extended with the Datafiable protocol"
    (let [rendered (-> (extend-datafy-class {:name "John Doe"}) inspect render)]
      (testing "renders the header"
        (is+ ["Class: "
              [:value "clojure.lang.PersistentArrayMap" 0]
              [:newline]
              "Count: 1"
              [:newline]
              [:newline]]
             (header rendered)))
      (testing "renders the meta information section"
        (is+ ["--- Meta Information:"
              [:newline]
              "  "
              [:value "clojure.core.protocols/datafy" 1]
              " = "
              [:value "#function[orchard.inspect-test/extend-datafy-class/fn]" 2]]
             (demunge (section rendered "Meta Information"))))
      (testing "renders the datafy section"
        (is+ ["--- Datafy:"
              [:newline]
              "  " [:value ":name" pos?] " = " [:value "\"John Doe\"" pos?]
              [:newline]
              "  " [:value ":class" pos?] " = " [:value "\"PersistentArrayMap\"" pos?]]
             (datafy-section rendered))))))

(deftest inspect-navigable-metadata-extension-test
  (testing "inspecting a map extended with the Navigable protocol"
    (let [rendered (-> (extend-nav-vector {:name "John Doe"}) inspect render)]
      (testing "renders the header"
        (is+ ["Class: "
              [:value "clojure.lang.PersistentArrayMap" 0]
              [:newline]
              "Count: 1"
              [:newline]
              [:newline]]
             (header rendered)))
      (testing "renders the meta information section"
        (is+ ["--- Meta Information:"
              [:newline]
              "  " [:value "clojure.core.protocols/nav" pos?]
              " = " [:value "#function[orchard.inspect-test/extend-nav-vector/fn]" pos?]]
             (demunge (section rendered "Meta Information"))))
      (testing "renders the datafy section"
        (is+ ["--- Datafy:"
              [:newline]
              "  " [:value ":name" pos?] " = " [:value "[:name \"John Doe\"]" pos?]]
             (datafy-section rendered))))))

(deftest inspect-throwable-test
  (testing "inspecting a throwable"
    (let [rendered (-> (doto ^Throwable (ex-info "BOOM" {})
                         (.setStackTrace (into-array StackTraceElement [])))
                       inspect render)]
      (testing "renders the header"
        (is+ ["Class: "
              [:value "clojure.lang.ExceptionInfo" 0]
              [:newline]
              "Message: BOOM"
              [:newline]
              [:newline]]
             (header rendered)))
      (testing "renders a causes section"
        (is+ ["--- Causes:"
              [:newline]
              "  BOOM" [:newline]
              "  " [:value "clojure.lang.ExceptionInfo" 1]]
             (section rendered "Causes")))
      (testing "renders the datafy section"
        (is+ (if (> java-api-version 8)
               ["--- Datafy:"
                [:newline]
                "  "
                [:value ":via" number?]
                " = "
                [:value
                 "[{:type clojure.lang.ExceptionInfo, :message \"BOOM\", :data {}}]"
                 number?]
                [:newline]
                "  "
                [:value ":trace" number?]
                " = "
                [:value "[]" number?]
                [:newline]
                "  "
                [:value ":cause" number?]
                " = "
                [:value "\"BOOM\"" number?]
                [:newline]
                "  "
                [:value ":data" number?]
                " = "
                [:value "{}" number?]]
               ["--- Datafy:"
                [:newline]
                "  " [:value ":via" number?] " = " [:value "[{:type clojure.lang.ExceptionInfo, :message \"BOOM\", :data {}}]" number?]
                [:newline]
                "  " [:value ":trace" number?] " = " [:value "[]" number?]
                [:newline]
                "  " [:value ":cause" number?] " = " [:value "\"BOOM\"" number?]
                [:newline]
                "  " [:value ":data" number?] " = " [:value "{}" number?]])
             (datafy-section rendered)))))

  (testing "exception with multiple causes"
    (let [rendered (-> (ex-info "Outer" {} (RuntimeException. "Inner"))
                       inspect render)]
      (is+ ["--- Causes:" [:newline]
            "  Outer" [:newline]
            "  " [:value "clojure.lang.ExceptionInfo" number?] " at "
            [:value #"orchard.inspect_test\$fn" number?] [:newline]
            [:newline]
            "  Inner" [:newline] "  " [:value "java.lang.RuntimeException" number?] " at "
            [:value #"orchard.inspect_test\$fn" number?]]
           (section rendered "Causes"))
      (testing "trace is rendered"
        (is+ (matchers/prefix
              ["--- Trace:" [:newline]
               "   0. " [:value #"orchard.inspect_test\$fn" number?] [:newline]
               "   1. " [:value #"orchard.inspect_test\$fn" number?] [:newline]
               "   2. " [:value string? number?] [:newline]
               "   3. " [:value string? number?] [:newline]
               "   4. " [:value string? number?] [:newline]])
             (section rendered "Trace"))))))

(deftest inspect-eduction-test
  (testing "inspecting eduction shows its object fields"
    (let [rendered (-> (eduction (range 10)) inspect render)]
      (testing "renders the header section"
        (is+ ["Class: "
              [:value "clojure.core.Eduction" 0]
              [:newline]
              "Value: "
              [:value "(0 1 2 3 4 ...)" 1]
              [:newline]
              #"^Identity hash code: "
              [:newline]
              [:newline]]
             (header rendered))))

    (let [rendered (-> (eduction (range 100)) inspect render)]
      (testing "doesn't render page info section"
        (is (nil? (section rendered "Page Info")))))))

(deftest render-counted-length-test
  (testing "inspecting counted collections shows their size upfront"
    (let [rendered (-> (range 10) inspect render)]
      (is+ ["Class: "
            [:value "clojure.lang.LongRange" 0]
            [:newline]
            "Count: 10"
            [:newline]
            [:newline]]
           (header rendered)))
    (let [rendered (-> (zipmap (range 20) (range 20)) inspect render)]
      (is+ ["Class: "
            [:value "clojure.lang.PersistentHashMap" 0]
            [:newline]
            "Count: 20"
            [:newline]
            [:newline]]
           (header rendered)))
    (let [rendered (-> (byte-array 30) inspect render)]
      (is+ ["Class: "
            [:value #"\[B|byte/1" 0]
            [:newline]
            "Count: 30"
            [:newline]
            "Component Type: " [:value "byte" 1]
            [:newline]
            [:newline]]
           (header rendered)))
    (let [rendered (-> (java.util.HashMap.) inspect render)]
      (is+ ["Class: "
            [:value "java.util.HashMap" 0]
            [:newline]
            "Count: 0"
            [:newline]
            [:newline]]
           (header rendered)))
    (let [rendered (-> (cons 1 (cons 2 nil)) inspect render)]
      (is+ ["Class: "
            [:value "clojure.lang.Cons" 0]
            [:newline]
            "Count: 2"
            [:newline]
            [:newline]]
           (header rendered)))))

(deftest object-view-mode-test
  (testing "in :object view-mode recognized objects are rendered as :default"
    (let [rendered (-> (list 1 2 3)
                       (inspect/start)
                       (inspect/set-view-mode :object)
                       render)]
      (is+ (matchers/prefix
            ["--- Instance fields:"
             [:newline]
             "  " [:value "_count" pos?] " = " [:value "3" pos?] [:newline]
             "  " [:value "_first" pos?] " = " [:value "1" pos?] [:newline]
             "  " [:value "_hash" pos?] " = " [:value "0" pos?] [:newline]])
           (section rendered "Instance fields"))
      (is+ [#"--- View mode" [:newline] "  normal ●object pretty"]
           (section rendered "View mode")))

    (let [rendered (-> (atom "foo")
                       (inspect/start)
                       (inspect/set-view-mode :object)
                       render)]
      (is+ (matchers/prefix
            ["--- Instance fields:"
             [:newline]
             "  " [:value "_meta" pos?] " = " [:value "nil" pos?] [:newline]
             "  " [:value "state" pos?] " = " [:value #"#object\[java.util.concurrent.atomic.AtomicReference" pos?] [:newline]
             "  " [:value "validator" pos?] " = " [:value "nil" pos?] [:newline]
             "  " [:value "watches" pos?] " = " [:value "{}" pos?]])
           (section rendered "Instance fields"))
      (is+ [#"--- View mode" [:newline] "  normal ●object pretty"]
           (section rendered "View mode"))))

  (testing "navigating away from an object changes the view mode back to normal"
    (is+ ["--- Contents:"
          [:newline]
          "  0. " [:value "2" pos?] [:newline]
          "  1. " [:value "3" pos?]]
         (-> (list 1 2 3)
             (inspect/start)
             (inspect/set-view-mode :object)
             (inspect/down 13)
             render
             contents-section))))

(deftest table-view-mode-test
  (testing "in :table view-mode lists of maps are rendered as tables"
    (let [rendered (-> (for [i (range 5)]
                         {:a (- i)
                          :bb (str i i i)
                          :ccc (range i 0 -1)})
                       (inspect/start)
                       (inspect/set-view-mode :table)
                       render)]
      (is+ ["--- Contents:" [:newline] [:newline]
            "  | " [:value "#" pos?] " | " [:value ":a" pos?] " |   "
            [:value ":bb" pos?] " |      " [:value ":ccc" pos?] " | " [:newline]
            "  |---+----+-------+-----------|" [:newline]
            "  | " [:value "0" pos?] " |  " [:value "0" pos?] " | "
            [:value "\"000\"" pos?] " |        " [:value "()" pos?] " | " [:newline]
            "  | " [:value "1" pos?] " | " [:value "-1" pos?] " | "
            [:value "\"111\"" pos?] " |       " [:value "(1)" pos?] " | " [:newline]
            "  | " [:value "2" pos?] " | " [:value "-2" pos?] " | "
            [:value "\"222\"" pos?] " |     " [:value "(2 1)" pos?] " | " [:newline]
            "  | " [:value "3" pos?] " | " [:value "-3" pos?] " | "
            [:value "\"333\"" pos?] " |   " [:value "(3 2 1)" pos?] " | " [:newline]
            "  | " [:value "4" pos?] " | " [:value "-4" pos?] " | "
            [:value "\"444\"" pos?] " | " [:value "(4 3 2 1)" pos?] " | "]
           (contents-section rendered))
      (is+ [#"--- View mode" [:newline] "  normal ●table object pretty"]
           (section rendered "View mode"))))

  (testing "in :table view-mode lists of vectors are rendered as tables"
    (let [rendered (-> (for [i (range 5)]
                         [(- i) (str i i i) (range i 0 -1)])
                       (inspect/start)
                       (inspect/set-view-mode :table)
                       render)]
      (is+ ["--- Contents:" [:newline] [:newline]
            "  | " [:value "#" pos?] " |  " [:value "0" pos?] " |     "
            [:value "1" pos?] " |         " [:value "2" pos?] " | " [:newline]
            "  |---+----+-------+-----------|" [:newline]
            "  | " [:value "0" pos?] " |  " [:value "0" pos?] " | "
            [:value "\"000\"" pos?] " |        " [:value "()" pos?] " | " [:newline]
            "  | " [:value "1" pos?] " | " [:value "-1" pos?] " | "
            [:value "\"111\"" pos?] " |       " [:value "(1)" pos?] " | " [:newline]
            "  | " [:value "2" pos?] " | " [:value "-2" pos?] " | "
            [:value "\"222\"" pos?] " |     " [:value "(2 1)" pos?] " | " [:newline]
            "  | " [:value "3" pos?] " | " [:value "-3" pos?] " | "
            [:value "\"333\"" pos?] " |   " [:value "(3 2 1)" pos?] " | " [:newline]
            "  | " [:value "4" pos?] " | " [:value "-4" pos?] " | "
            [:value "\"444\"" pos?] " | " [:value "(4 3 2 1)" pos?] " | "]
           (contents-section rendered))
      (is+ [#"--- View mode" [:newline] "  normal ●table object pretty"]
           (section rendered "View mode"))))

  (testing "breaks if table mode is requested for unsupported value"
    (is (thrown? Exception (-> {:a 1}
                               (inspect/start)
                               (inspect/set-view-mode :table)
                               render
                               contents-section))))

  (testing "works with paging"
    (is+ ["--- Contents:" [:newline] [:newline]
          "  | " [:value "#" pos?] " | " [:value "0" pos?] " | " [:value "1" pos?] " | " [:newline]
          "  |---+---+---|" [:newline]
          "  | " [:value "0" pos?] " | " [:value "0" pos?] " | " [:value "0" pos?] " | " [:newline]
          "  | " [:value "1" pos?] " | " [:value "1" pos?] " | " [:value "1" pos?] " | " [:newline]
          "  | " [:value "2" pos?] " | " [:value "2" pos?] " | " [:value "2" pos?] " | " [:newline]
          "  ..."]
         (-> (map #(vector % %) (range 9))
             (inspect/start)
             (set-page-size 3)
             (inspect/set-view-mode :table)
             render
             contents-section))

    (is+ ["--- Contents:" [:newline]
          "  ..." [:newline] [:newline]
          "  | " [:value "#" pos?] " | " [:value "0" pos?] " | " [:value "1" pos?] " | " [:newline]
          "  |---+---+---|" [:newline]
          "  | " [:value "3" pos?] " | " [:value "3" pos?] " | " [:value "3" pos?] " | " [:newline]
          "  | " [:value "4" pos?] " | " [:value "4" pos?] " | " [:value "4" pos?] " | " [:newline]
          "  | " [:value "5" pos?] " | " [:value "5" pos?] " | " [:value "5" pos?] " | " [:newline]
          "  ..."]
         (-> (map #(vector % %) (range 9))
             (inspect/start)
             (set-page-size 3)
             (inspect/next-page)
             (inspect/set-view-mode :table)
             render
             contents-section))

    (is+ ["--- Contents:" [:newline]
          "  ..." [:newline] [:newline]
          "  | " [:value "#" pos?] " | " [:value "0" pos?] " | " [:value "1" pos?] " | " [:newline]
          "  |---+---+---|" [:newline]
          "  | " [:value "6" pos?] " | " [:value "6" pos?] " | " [:value "6" pos?] " | " [:newline]
          "  | " [:value "7" pos?] " | " [:value "7" pos?] " | " [:value "7" pos?] " | " [:newline]
          "  | " [:value "8" pos?] " | " [:value "8" pos?] " | " [:value "8" pos?] " | "]
         (-> (map #(vector % %) (range 9))
             (inspect/start)
             (set-page-size 3)
             (inspect/next-page)
             (inspect/next-page)
             (inspect/set-view-mode :table)
             render
             contents-section)))

  (testing "map is not reported as table-viewable when paged"
    (is (not (-> (zipmap (range 100) (range))
                 (inspect/start)
                 (set-page-size 30)
                 (inspect/view-mode-supported? :table))))))

(deftest hex-view-mode-test
  (testing "in :hex view-mode byte arrays are rendered as hexdump tables"
    (let [rendered (-> (byte-array (range 100))
                       inspect
                       (inspect/set-view-mode :hex)
                       render)]
      (is+ ["--- Contents:" [:newline]
            "  0x00000000 │ 00 01 02 03 04 05 06 07  08 09 0a 0b 0c 0d 0e 0f │ ················" [:newline]
            "  0x00000010 │ 10 11 12 13 14 15 16 17  18 19 1a 1b 1c 1d 1e 1f │ ················" [:newline]
            "  0x00000020 │ 20 21 22 23 24 25 26 27  28 29 2a 2b 2c 2d 2e 2f │  !\"#$%&'()*+,-./" [:newline]
            "  0x00000030 │ 30 31 32 33 34 35 36 37  38 39 3a 3b 3c 3d 3e 3f │ 0123456789:;<=>?" [:newline]
            "  0x00000040 │ 40 41 42 43 44 45 46 47  48 49 4a 4b 4c 4d 4e 4f │ @ABCDEFGHIJKLMNO" [:newline]
            "  0x00000050 │ 50 51 52 53 54 55 56 57  58 59 5a 5b 5c 5d 5e 5f │ PQRSTUVWXYZ[\\]^_" [:newline]
            "  0x00000060 │ 60 61 62 63                                      │ `abc"]
           (contents-section rendered))
      (is+ [#"--- View mode" [:newline] "  normal ●hex object pretty"]
           (section rendered "View mode"))))

  (testing "works with paging"
    (is+ ["--- Contents:" [:newline]
          "  0x00000000 │ 00 01 02 03 04 05 06 07  08 09 0a 0b 0c 0d 0e 0f │ ················" [:newline]
          "  0x00000010 │ 10 11 12 13 14 15 16 17  18 19 1a 1b 1c 1d 1e 1f │ ················" [:newline]
          "  ..."]
         (-> (byte-array (range 100))
             inspect
             (inspect/set-view-mode :hex)
             (set-page-size 2)
             render
             contents-section))

    (is+ ["--- Contents:" [:newline]
          "  ..." [:newline]
          "  0x00000020 │ 20 21 22 23 24 25 26 27  28 29 2a 2b 2c 2d 2e 2f │  !\"#$%&'()*+,-./" [:newline]
          "  0x00000030 │ 30 31 32 33 34 35 36 37  38 39 3a 3b 3c 3d 3e 3f │ 0123456789:;<=>?" [:newline]
          "  ..."]
         (-> (byte-array (range 100))
             inspect
             (inspect/set-view-mode :hex)
             (set-page-size 2)
             inspect/next-page
             render
             contents-section))))

(deftest toggle-view-mode-test
  (is+ :normal (-> (repeat 10 [1 2]) inspect :view-mode))
  (is+ "  ●normal table object pretty"
       (-> (repeat 10 [1 2]) inspect render (section "View mode") last))

  (is+ :table (-> (repeat 10 [1 2]) inspect inspect/toggle-view-mode :view-mode))
  (is+ "  normal ●table object pretty"
       (-> (repeat 10 [1 2]) inspect inspect/toggle-view-mode render (section "View mode") last))

  (is+ :object (-> (repeat 10 [1 2]) inspect inspect/toggle-view-mode inspect/toggle-view-mode :view-mode))
  (is+ "  normal table ●object pretty"
       (-> (repeat 10 [1 2]) inspect inspect/toggle-view-mode inspect/toggle-view-mode render (section "View mode") last))

  (is+ :normal (-> (repeat 10 [1 2]) inspect inspect/toggle-view-mode inspect/toggle-view-mode inspect/toggle-view-mode :view-mode))

  (is+ "  ●normal table object ●pretty"
       (-> (inspect {:pretty-print true} (repeat 10 [1 2])) render (section "View mode") last)))

(deftest pretty-print-map-test
  (testing "in :pretty view-mode are pretty printed"
    (let [rendered (-> {:a 0
                        :bb "000"
                        :ccc []
                        :d [{:a 0 :bb "000" :ccc [[]]}
                            {:a -1 :bb "111" :ccc [1]}
                            {:a 2 :bb "222" :ccc [1 2]}]}
                       (inspect/start)
                       (set-pretty-print true)
                       render)]
      (is+ ["--- Contents:" [:newline] "  "
            [:value ":a" 1] " = " [:value "0" 2]
            [:newline] "  "
            [:value ":bb" 3] " = " [:value "\"000\"" 4]
            [:newline] "  "
            [:value ":ccc" 5] " = " [:value "[]" 6]
            [:newline] "  "
            [:value ":d" 7] " = "
            [:value (str "[{:a 0, :bb \"000\", :ccc [[]]}\n"
                         "        {:a -1, :bb \"111\", :ccc [1]}\n"
                         "        {:a 2, :bb \"222\", :ccc [1 2]}]") 8]]
           (contents-section rendered))
      (is+ [#"--- View mode" [:newline] "  ●normal object ●pretty"]
           (section rendered "View mode")))))

(deftest pretty-print-map-in-object-view-test
  (testing "in :object view mode + :pretty, Value: is printed regularly"
    (let [rendered (-> {:a 0
                        :bb "000"
                        :ccc []
                        :d [{:a 0 :bb "000" :ccc [[]]}
                            {:a -1 :bb "111" :ccc [1]}
                            {:a 2 :bb "222" :ccc [1 2]}]}
                       (inspect/start)
                       (inspect/set-view-mode :object)
                       (set-pretty-print true)
                       render)]
      (is+ ["Value: "
            [:value "{:a 0, :bb \"000\", :ccc [], :d [{:a 0, :bb \"000\", :ccc [[]]} {:a -1, :bb \"111\", :ccc [1]} {:a 2, :bb \"222\", :ccc [1 2]}]}" 1]]
           (labeled-value "Value" rendered)))))

(deftest pretty-print-seq-of-maps-test
  (testing "in :pretty view-mode maps seqs of maps are pretty printed"
    (let [rendered (-> (for [i (range 2)]
                         {:a (- i)
                          :bb (str i i i)
                          :ccc (range i 0 -1)
                          :d (for [i (range 5)]
                               {:a (- i)
                                :bb (str i i i)
                                :ccc (range i 0 -1)})})
                       (inspect/start)
                       (set-pretty-print true)
                       render)]
      (is+ ["--- Contents:" [:newline]
            "  0. "
            [:value (str "{:a 0,\n      :bb \"000\",\n      :ccc (),\n      "
                         ":d\n      ({:a 0, :bb \"000\", :ccc ()}\n       "
                         "{:a -1, :bb \"111\", :ccc (1)}\n       {:a -2, :bb "
                         "\"222\", :ccc (2 1)}\n       {:a -3, :bb \"333\", "
                         ":ccc (3 2 1)}\n       {:a -4, :bb \"444\", :ccc "
                         "(4 3 2 1)})}") 1]
            [:newline]
            "  1. "
            [:value (str "{:a -1,\n      :bb \"111\",\n      :ccc (1),\n      "
                         ":d\n      ({:a 0, :bb \"000\", :ccc ()}\n       "
                         "{:a -1, :bb \"111\", :ccc (1)}\n       {:a -2, :bb "
                         "\"222\", :ccc (2 1)}\n       {:a -3, :bb \"333\", "
                         ":ccc (3 2 1)}\n       {:a -4, :bb \"444\", "
                         ":ccc (4 3 2 1)})}") 2]]
           (contents-section rendered))
      (is+ [#"--- View mode" [:newline] "  ●normal table object ●pretty"]
           (section rendered "View mode")))))

(deftest pretty-print-map-as-key-test
  (testing "in :pretty view-mode maps that contain maps as a keys are pretty printed"
    (let [rendered (-> {{:a 0
                         :bb "000"
                         :ccc []
                         :d [{:a 0 :bb "000" :ccc []}
                             {:a -1 :bb "111" :ccc [1]}
                             {:a -2 :bb "222" :ccc [2 1]}
                             {:a -3 :bb "333" :ccc [3 2 1]}
                             {:a -4 :bb "444" :ccc [4 3 2 1]}]}
                        {:a -1
                         :bb "111"
                         :ccc [1]
                         :d [{:a 0 :bb "000" :ccc []}
                             {:a -1 :bb "111" :ccc [1]}
                             {:a -2 :bb "222" :ccc [2 1]}
                             {:a -3 :bb "333" :ccc [3 2 1]}
                             {:a -4 :bb "444" :ccc [4 3 2 1]}]}}
                       (inspect/start)
                       (set-pretty-print true)
                       render)]
      (is+ ["--- Contents:" [:newline] "  "
            [:value (str "{:a 0,\n   :bb \"000\",\n   :ccc [],\n   :d\n   "
                         "[{:a 0, :bb \"000\", :ccc []}\n    {:a -1, "
                         ":bb \"111\", :ccc [1]}\n    {:a -2, :bb \"222\", "
                         ":ccc [2 1]}\n    {:a -3, :bb \"333\", :ccc [3 2 1]}"
                         "\n    {:a -4, :bb \"444\", :ccc [4 3 2 1]}]}") 1]
            [:newline] "  =" [:newline] "  "
            [:value (str "{:a -1,\n   :bb \"111\",\n   :ccc [1],\n   "
                         ":d\n   [{:a 0, :bb \"000\", :ccc []}\n    "
                         "{:a -1, :bb \"111\", :ccc [1]}\n    {:a -2, "
                         ":bb \"222\", :ccc [2 1]}\n    {:a -3, :bb "
                         "\"333\", :ccc [3 2 1]}\n    {:a -4, :bb "
                         "\"444\", :ccc [4 3 2 1]}]}") 2]]
           (contents-section rendered))
      (is+ [#"--- View mode" [:newline] "  ●normal object ●pretty"]
           (section rendered "View mode")))))

(deftest pretty-print-seq-of-map-as-key-test
  (testing "in :pretty view-mode maps that contain seq of maps as a keys are pretty printed"
    (let [rendered (-> {[{:a 0
                          :bb "000"
                          :ccc []
                          :d [{:a 0 :bb "000" :ccc [[]]}
                              {:a -1 :bb "111" :ccc [1]}
                              {:a 2 :bb "222" :ccc [1 2]}]}]
                        {:a 0
                         :bb "000"
                         :ccc []
                         :d [{:a 0 :bb "000" :ccc [[]]}
                             {:a -1 :bb "111" :ccc [1]}
                             {:a 2 :bb "222" :ccc [1 2]}]}}
                       (inspect/start)
                       (set-pretty-print true)
                       render)]
      (is+ ["--- Contents:" [:newline] "  "
            [:value (str "[{:a 0,\n    :bb \"000\",\n    :ccc [],\n    :d\n    "
                         "[{:a 0, :bb \"000\", :ccc [[]]}\n     {:a -1, :bb \"111\", "
                         ":ccc [1]}\n     {:a 2, :bb \"222\", :ccc [1 2]}]}]") 1]
            [:newline] "  =" [:newline] "  "
            [:value (str "{:a 0,\n   :bb \"000\",\n   :ccc [],\n   :d\n   "
                         "[{:a 0, :bb \"000\", :ccc [[]]}\n    {:a -1, "
                         ":bb \"111\", :ccc [1]}\n    {:a 2, :bb \"222\", "
                         ":ccc [1 2]}]}") 2]]
           (contents-section rendered))
      (is+ [#"--- View mode" [:newline] "  ●normal object ●pretty"]
           (section rendered "View mode")))))

(deftest tap-test
  (testing "tap-current-value"
    (let [proof (atom [])
          test-tap-handler (fn [x]
                             (swap! proof conj x))
          sleep (long
                 (if (System/getenv "CI")
                   200
                   100))]

      (add-tap test-tap-handler)

      (-> (inspect {:a {:b 1}})
          (inspect/tap-current-value)
          (inspect/down 2)
          (inspect/tap-current-value)
          (inspect/down 1)
          (inspect/tap-current-value))

      (let [expected [{:a {:b 1}}
                      {:b 1}
                      :b]
            tries (atom 0)]

        (while (and (not= expected @proof)
                    (< @tries 1000))
          (Thread/sleep sleep)
          (swap! tries inc))

        (is (= expected @proof)))

      (remove-tap test-tap-handler)))

  (testing "tap-indexed"
    (let [proof (atom [])
          test-tap-handler (fn [x]
                             (swap! proof conj x))
          sleep (long
                 (if (System/getenv "CI")
                   200
                   100))]

      (add-tap test-tap-handler)

      (-> (inspect {:a {:b 1}})
          (inspect/tap-indexed 1)
          (inspect/tap-indexed 2)
          (inspect/down 2)
          (inspect/tap-indexed 1)
          (inspect/tap-indexed 2))

      (let [expected [:a
                      {:b 1}
                      :b
                      1]
            tries (atom 0)]

        (while (and (not= expected @proof)
                    (< @tries 1000))
          (Thread/sleep sleep)
          (swap! tries inc))

        (is (= expected @proof)))

      (remove-tap test-tap-handler))))

(deftest datafy-test
  (testing "When `(datafy x)` is identical to `x`, no Datafy section is included"
    (is+ nil (-> {:foo :bar} inspect render datafy-section))
    (is+ nil (-> {:foo :bar :nilable nil} inspect render datafy-section)))
  (testing "datafy is not included for records"
    (is+ nil (-> (->TestRecord 1 2 3 4) inspect render datafy-section)))
  (testing "if datafied repr doesn't mirror the original, don't page datafied"
    (let [rendered (-> {:a 1, :b 2}
                       (with-meta {'clojure.core.protocols/datafy
                                   (fn [_] (range 30))})
                       inspect
                       (set-page-size 1)
                       render)]
      (is+ ["--- Contents:"
            [:newline]
            "  " [:value ":a" pos?]
            " = "
            [:value "1" pos?]
            [:newline]
            "  ..."]
           (contents-section rendered))
      (is+ ["--- Datafy:"
            [:newline]
            "  " [:value "[0 1 2 3 4 ...]" pos?]]
           (datafy-section rendered)))

    (testing "if datafied is small enough, render it as a collection"
      (let [rendered (-> {:a 1, :b 2}
                         (with-meta {'clojure.core.protocols/datafy
                                     (fn [_] (range 3))})
                         inspect
                         (set-page-size 5)
                         render)]
        (is+ ["--- Datafy:"
              [:newline]
              "  0. " [:value "0" pos?]
              [:newline]
              "  1. " [:value "1" pos?]
              [:newline]
              "  2. " [:value "2" pos?]]
             (datafy-section rendered)))))
  (testing "datafy doesn't show if the differing datafied is not on the current page"
    (let [ins (-> {:a 1, :b (with-meta [] {'clojure.core.protocols/datafy
                                           (fn [_] :datafied)})}
                  inspect
                  (set-page-size 1))
          rendered (render ins)]
      (is+ nil (datafy-section rendered))
      (is+ ["--- Datafy:" [:newline]
            "  ..." [:newline]
            "  " [:value ":b" pos?] " = " [:value ":datafied" pos?]]
           (datafy-section (-> ins (inspect/next-page) render))))
    (let [ins (-> [1 2 3 (with-meta [] {'clojure.core.protocols/datafy
                                        (fn [_] :datafied)})]
                  inspect
                  (set-page-size 2))
          rendered (render ins)]
      (is+ nil (datafy-section rendered))
      (is+ ["--- Datafy:" [:newline]
            "  ..." [:newline]
            "  3. " [:value ":datafied" pos?]]
           (datafy-section (-> ins inspect/next-page render)))))
  (testing "only show those items in collection that have unique datafication"
    (is+ ["--- Datafy:" [:newline]
          "  3. " [:value string? pos?]]
         (-> [1 2 3 (ex-info "datafy" {})]
             inspect render datafy-section))
    (is+ ["--- Datafy:" [:newline]
          "  " [:value ":c" pos?] " = " [:value string? pos?]]
         (-> {:a 1 :b 2 :c (ex-info "datafy" {})}
             inspect render datafy-section))))

(deftest private-field-access-test
  (testing "Inspection of private fields is attempted (may fail depending on the JDK and the module of the given class)"
    (if (< java-api-version 17)
      (do
        (is+ nil (-> 2 inspect render (section "Private static fields")))
        (is+ (matchers/embeds [[:value "serialVersionUID" number?]])
             (-> 2 inspect render (section "Static fields"))))

      (is+ ["--- Private static fields:"
            [:newline]
            "  "
            [:value "serialVersionUID" number?]
            " = "
            [:value "<non-inspectable value>" number?]]
           (-> 2 inspect render (section "Private static fields"))))

    (testing "Fully inspects private fields for a class that is module-accessible"
      (is+ ["--- Instance fields:"
            [:newline]
            "  "
            [:value "age" number?]
            " = "
            [:value "42" number?]]
           (-> (PrivateFieldClass. 42) inspect render (section "Instance fields"))))))

(deftest analytics-test
  (testing "analytics is not shown by default"
    (is+ nil (-> (range 100) inspect render (section "Analytics"))))

  (testing "analytics hint is displayed if requested"
    (is+ ["--- Analytics:" [:newline]
          "  Press 'y' or M-x cider-inspector-display-analytics to analyze this value."]
         (-> (inspect {:display-analytics-hint "true"} (range 100)) render
             (section "Analytics"))))

  (testing "analytics is shown when requested"
    (is+ ["--- Analytics:" [:newline]
          "  " [:value ":count" pos?] " = " [:value "100" pos?] [:newline]
          "  " [:value ":types" pos?] " = " [:value "{java.lang.Long 100}" pos?] [:newline]
          "  " [:value ":frequencies" pos?] " = " [:value string? pos?] [:newline]
          "  " [:value ":numbers" pos?] " = " [:value "{:n 100, :zeros 1, :max 99, :min 0, :mean 49.5}" pos?]]
         (-> (range 100) inspect inspect/display-analytics render (section "Analytics"))))

  (testing "cutoff is customizable and limits number of values analytics processes"
    (is+ (matchers/prefix
          ["--- Analytics:" [:newline]
           "  " [:value ":cutoff?" pos?] " = " [:value "true" pos?] [:newline]
           "  " [:value ":count" pos?] " = " [:value "10" pos?] [:newline]
           "  " [:value ":types" pos?] " = " [:value "{java.lang.Long 10}" pos?]])
         (-> (range 100)
             inspect
             (inspect/refresh {:analytics-size-cutoff 10})
             inspect/display-analytics
             render
             (section "Analytics")))))
