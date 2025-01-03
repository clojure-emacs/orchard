(ns orchard.apropos-test
  (:require
   [clojure.repl :as repl]
   [clojure.string :as string]
   [clojure.test :refer [are deftest is testing]]
   [orchard.apropos :as sut :refer [find-symbols]]
   [orchard.meta :refer [var-name var-doc]]))

(def ^{:doc "Test1. Test2. Test3."} public-var [1 2 3])

(defn some-random-function [])

(deftest apropos-sort-test
  (doseq [namespace (all-ns)]
    (let [vars (->> namespace ns-interns vals)]
      (is (sut/apropos-sort namespace vars)
          "Doesn't throw errors"))))

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
  (testing "Namespace sort order"
    (let [ns (find-ns 'orchard.apropos-test)]
      (is (= ns (-> (find-symbols {:ns ns}) first :name symbol namespace symbol find-ns))
          "Current namespace should be first."))
    (is (-> (find-symbols nil) first :name symbol namespace
            (.startsWith "clojure."))
        "Absent a current namespace, clojure.* should be first."))

  (testing "Removal of namespaces with `exclude-regexps`"
    (is (not-any? #(some-> (namespace (symbol (:name %)))
                           (string/includes? "orchard"))
                  (find-symbols {:var-query
                                 {:ns-query
                                  {:exclude-regexps
                                   [#"orchard"]}}})))))

(defn- apropos-first
  ([v]
   (apropos-first v nil))
  ([v search-ns]
   (->> (find-symbols
         (cond-> {:var-query {:search (re-pattern (string/escape v {\* "\\*"}))}}
           search-ns
           (assoc-in [:var-query :ns-query :exactly] [search-ns])))
        (filter #(= (:name %) (if search-ns (format "%s/%s" search-ns v) v)))
        first)))

(deftest search-test
  (testing "Search results"
    (is (empty? (find-symbols {:var-query {:search #"xxxxxxxx"}}))
        "Failing searches should return empty.")
    (is (= 1 (count (find-symbols {:var-query {:search #"some-random-function"}})))
        "Search for specific fn should return it.")
    (doseq [private? [true false]]
      (is (< 1000
             (count (find-symbols {:var-query {:search #".*"
                                               :private? private?}})))
          "Everything is searchable; it won't throw errors")))

  (testing "Types are correct"
    (is (= :special-form (:type (apropos-first "def"))))
    (are [var type] (= type (:type (apropos-first var (the-ns 'clojure.core))))
      "when" :macro
      "reduce" :function
      "print-method" :function
      "*print-length*" :variable))

  (testing "Symbol vs docstring search"
    ;; Search for the same fn by name and docstring
    (let [x (first (find-symbols
                    {:var-query {:search #"find-symbols$"}}))
          y (first (find-symbols
                    {:var-query {:search #"Causes the full doc to be returned instead"
                                 :search-property :doc}
                     :full-doc? true}))]
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
                           (find-symbols {:search #"if"})))))

  (testing "Includes special forms when `search-ns` is \"clojure.core\""
    (is (not-empty (filter #(= "if" (:name %))
                           (find-symbols {:search #"if"
                                          :ns-query {:exactly [(the-ns 'clojure.core)]}})))))

  (testing "Excludes special forms when `search-ns` is some other ns"
    (is (empty? (filter #(= "if" (:name %))
                        (find-symbols {:var-query
                                       {:search #"if"
                                        :ns-query {:exactly [(the-ns 'clojure.set)]}}}))))))
