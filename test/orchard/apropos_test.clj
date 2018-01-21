(ns orchard.apropos-test
  (:require [orchard.apropos :refer :all]
            [orchard.meta :refer [var-name var-doc]]
            [clojure.test :refer :all]
            [clojure.repl :as repl]
            [clojure.string :as str]))

(def ^{:doc "Test1. Test2. Test3."} public-var [1 2 3])
(def ^:private ^{:doc "Can't. See. Me"} private-var [:a :b :c])

(defn find-symbols1 [ns query search-ns docs? privates? case-sensitive? filter-regexps]
  (find-symbols {:ns ns :query query :search-ns search-ns
                 :docs? docs? :privates? privates?
                 :case-sensitive? case-sensitive?
                 :filter-regexps filter-regexps}))

(deftest var-name-test
  (testing "Returns Var's namespace-qualified name"
    (is (= "clojure.core/conj" (var-name #'clojure.core/conj))))

  (testing "Returns special form's name"
    (is (= "if" (var-name 'if)))))

(deftest var-doc-test
  (testing "Returns Var's doc"
    (is (= (:doc (meta #'clojure.core/conj))
           (var-doc #'clojure.core/conj))))

  (testing "Returns special form's doc"
    (is (= (:doc (#'repl/special-doc 'if))
           (var-doc 'if)))))

(deftest unit-test-metadata
  (is (= (var-name  #'public-var) "orchard.apropos-test/public-var"))
  (is (= (var-doc   #'public-var) "Test1. Test2. Test3."))
  (is (= (var-doc 1 #'public-var) "Test1.")))

(deftest namespaces-test
  (let [ns (-> *ns* ns-name str)]
    (testing "Namespace sort order"
      (is (= (-> (namespaces ns nil) first ns-name str)
             ns)
          "Current namespace should be first.")
      (is (-> (namespaces nil nil) first ns-name str
              (.startsWith "clojure."))
          "Absent a current namespace, clojure.* should be first."))

    (testing "Searched namespace"
      (is (= (namespaces ns ns)
             (namespaces nil ns)
             (list (find-ns (symbol ns))))
          "Should return a list containing only the searched ns."))

    (testing "Removal of namespaces with `filter-regexps`"
      (is (not-any? #(re-find #".*orchard" (str (ns-name %)))
                    (namespaces nil nil [".*orchard"]))))))

(deftest search-test
  (testing "Search results"
    (is (empty? (find-symbols1 nil "xxxxxxxx" nil false false false nil))
        "Failing searches should return empty.")
    (is (= 1 (count (find-symbols1 nil "find-symbols1" nil false false false nil)))
        "Search for specific fn should return it."))

  (testing "Symbol vs docstring search"
    ;; Search for the same fn by name and docstring
    (let [x (first (find-symbols1 nil "find-symbols" nil false false false nil))
          y (first (find-symbols1 nil "The search may optionally include private"
                                  nil true false false nil))]
      (is (= (dissoc x :doc)
             (dissoc y :doc))
          "Other than docstring, returned attributes should be the same.")
      (is (< (count (:doc x))
             (count (:doc y)))
          "Symbol search should return an abbreviated docstring.")
      (is (= (take 20 (:doc x))
             (take 20 (:doc y)))
          "The abbreviated docstring should be the start of the full docstring.")))

  (testing "Includes special forms when `search-ns` is nil"
    (is (not-empty (filter #(= "if" (:name %))
                           (find-symbols1 nil "if" nil
                                          false false false nil)))))

  (testing "Includes special forms when `search-ns` is \"clojure.core\""
    (is (not-empty (filter #(= "if" (:name %))
                           (find-symbols1 nil "if" "clojure.core"
                                          false false false nil)))))

  (testing "Excludes special forms when `search-ns` is some other ns"
    (is (empty? (filter #(= "if" (:name %))
                        (find-symbols1 nil "if" "clojure.set"
                                       false false false nil))))))
