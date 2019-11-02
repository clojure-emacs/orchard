(ns orchard.cljs.env-test
  (:require [clojure.set :as set]
            [clojure.test :as test #?(:clj :refer :cljs :refer-macros) [deftest is testing use-fixtures]]
            [orchard.cljs.analysis :as a]
            [orchard.cljs.test-env :as test-env]))

(deftest test-env
  (let [env (test-env/create-test-env)]
    (testing "Test environment"
      (is (empty? (set/difference (set (keys (a/all-ns env)))
                                  '#{orchard.test-ns
                                     orchard.test-ns-dep
                                     orchard.test-no-defs
                                     orchard.test-macros
                                     cljs.core
                                     cljs.user
                                     clojure.set
                                     clojure.string}))))))
