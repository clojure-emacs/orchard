(ns orchard.cljs.meta-test
  "JVM-side tests for the pure normalization functions of `orchard.cljs.meta`.
  See test-cljs/orchard/cljs/meta_test.cljc for the ClojureScript variant."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [orchard.cljs.meta :as cljs-meta]))

(deftest unquote-1-test
  (is (= [1 2 3] (cljs-meta/unquote-1 '(quote [1 2 3]))))
  (is (= [1 2 3] (cljs-meta/unquote-1 [1 2 3])))
  (is (= nil (cljs-meta/unquote-1 nil))))

(deftest normalize-ns-file-test
  (testing "takes the file of the first def when present"
    (is (= "orchard/fake.cljs"
           (cljs-meta/normalize-ns-file {:defs {'x {:file "orchard/fake.cljs"}}}))))
  (testing "falls back to the canonical source of the namespace"
    (is (str/ends-with? (cljs-meta/normalize-ns-file {:name 'orchard.cljs.meta})
                        "orchard/cljs/meta.cljc"))))

(deftest normalize-ns-meta-test
  (is (= {:doc "docstring"
          :author "someone"
          :file "orchard/fake.cljs"
          :line 1
          :name 'orchard.fake
          :ns 'orchard.fake}
         (cljs-meta/normalize-ns-meta
          {:name 'orchard.fake
           :doc "docstring"
           :author "someone"
           :extraneous :key
           :defs {'x {:file "orchard/fake.cljs"}}}))))

(deftest normalize-var-meta-test
  (is (= '{:name foo, :arglists ([x] [x y])}
         (cljs-meta/normalize-var-meta '{:name foo, :arglists (quote ([x] [x y]))}))))

(deftest normalize-macro-meta-test
  (let [normalized (cljs-meta/normalize-macro-meta
                    '{:name my-macro
                      :ns cljs.fake
                      :file "cljs/fake.cljc"
                      :arglists (quote ([x]))
                      :meta {:file "wrong/file.cljc", :doc "docstring"}})]
    (testing ":file, :ns and :name win over the nested :meta"
      (is (= "cljs/fake.cljc" (:file normalized)))
      (is (= 'cljs.fake (:ns normalized)))
      (is (= 'my-macro (:name normalized))))
    (testing "the nested :meta map is merged in"
      (is (= "docstring" (:doc normalized))))
    (testing "arglists are unquoted"
      (is (= '([x]) (:arglists normalized))))))
