(ns orchard.apropos-test
  (:require
   [clojure.repl :as repl]
   [clojure.string :as str]
   [clojure.test :refer [are deftest is testing]]
   [matcher-combinators.matchers :as mc]
   [orchard.apropos :as sut :refer [find-symbols]]
   [orchard.meta :refer [var-doc var-name]]
   [orchard.test.util :refer [is+]]))

(def ^{:doc "Test1. Test2. Test3."} public-var [1 2 3])

(defn some-random-function [])

(deftest apropos-sort-test
  (testing "sorts first by the given ns, then clojure namespaces, then the rest, all alphabetically"
    (is+ [#'some-random-function #'+ #'str #'find-symbols]
         (sut/apropos-sort (find-ns 'orchard.apropos-test)
                           [#'str #'find-symbols #'some-random-function #'+])))

  (testing "doesn't throw errors"
    (is (sut/apropos-sort *ns* (mapcat (comp vals ns-interns) (all-ns))))))

(deftest var-name-test
  (testing "Returns Var's namespace-qualified name"
    (is+ "clojure.core/conj" (var-name #'clojure.core/conj)))

  (testing "Returns special form's name"
    (is+ "if" (var-name 'if))))

(deftest var-doc-test
  (testing "Returns Var's doc"
    (is+ (:doc (meta #'clojure.core/conj))
         (var-doc #'clojure.core/conj)))

  (testing "Returns special form's doc"
    (is+ (:doc (#'repl/special-doc 'if))
         (var-doc 'if))))

(deftest unit-test-metadata
  (is+ "orchard.apropos-test/public-var" (var-name #'public-var))
  (is+ "Test1. Test2. Test3." (var-doc #'public-var))
  (is+ "Test1." (var-doc 1 #'public-var)))

(deftest namespaces-test
  (testing "Namespace sort order"
    (is+ (mc/prefix [{:name #"^orchard\.apropos-test/"}])
         (find-symbols {:ns (find-ns 'orchard.apropos-test)})
         "Current namespace should be first.")
    (is+ (mc/prefix [{:name #"^clojure\."}])
         (find-symbols nil)
         "Absent a current namespace, clojure.* should be first."))

  (testing "Removal of namespaces with `exclude-regexps`"
    (is+ (mc/seq-of {:name #(not (str/starts-with? % "orchard"))})
         (find-symbols {:var-query
                        {:ns-query
                         {:exclude-regexps
                          [#"orchard"]}}}))))

(defn- apropos-first
  ([v]
   (apropos-first v nil))
  ([v search-ns]
   (->> (find-symbols
         (cond-> {:var-query {:search (re-pattern (str/escape v {\* "\\*"}))}}
           search-ns
           (assoc-in [:var-query :ns-query :exactly] [search-ns])))
        (filter #(= (:name %) (if search-ns (format "%s/%s" search-ns v) v)))
        first)))

(deftest search-test
  (testing "Search results"
    (is+ [] (find-symbols {:var-query {:search #"xxxxxxxx"}})
         "Failing searches should return empty.")
    (is+ [some?] (find-symbols {:var-query {:search #"some-random-function"}})
         "Search for specific fn should return it.")
    (doseq [private? [true false]]
      (is (seq (find-symbols {:var-query {:search #".*"
                                          :private? private?}}))
          "Everything is searchable; it won't throw errors")))

  (testing "Types are correct"
    (is+ {:type :special-form} (apropos-first "def"))
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
      (is (str/starts-with? (:doc y) (:doc x))
          "Symbol search should return an abbreviated docstring.")))

  (testing "Includes special forms when `search-ns` is nil"
    (is+ (mc/embeds [{:name "if"}])
         (find-symbols {:var-query {:search #"if"}})))

  (testing "Includes special forms when `search-ns` is \"clojure.core\""
    (is+ (mc/embeds [{:name "if"}])
         (find-symbols {:var-query
                        {:search #"if"
                         :ns-query {:exactly [(the-ns 'clojure.core)]}}})))

  (testing "Excludes special forms when `search-ns` is some other ns"
    (is+ (mc/mismatch (mc/embeds [{:name "if"}]))
         (find-symbols {:var-query
                        {:search #"if"
                         :ns-query {:exactly [(the-ns 'clojure.set)]}}}))))
