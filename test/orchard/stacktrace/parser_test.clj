(ns orchard.stacktrace.parser-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [orchard.stacktrace.parser :as parser]
   [orchard.stacktrace.parser.test :as test]))

(deftest parse-input-transformation-test
  (let [text (test/read-fixture :boom.java)]
    (testing "transformation of pr-str 1 level deep"
      (is (= (parser/parse text) (parser/parse (pr-str text)))))
    (testing "transformation of pr-str 2 levels deep"
      (is (= (parser/parse text) (parser/parse (pr-str (pr-str text))))))))
