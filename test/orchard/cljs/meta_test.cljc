(ns orchard.cljs.meta-test
  (:require [clojure.test :as test #?(:clj :refer :cljs :refer-macros) [deftest is testing]]
            [orchard.cljs.meta :as cljs-meta]))

(deftest unquote-test
  (is (= [1 2 3] (cljs-meta/unquote-1 '(quote [1 2 3]))))
  (is (= [1 2 3] (cljs-meta/unquote-1 [1 2 3])))
  (is (= nil (cljs-meta/unquote-1 nil))))
