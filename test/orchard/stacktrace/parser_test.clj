(ns orchard.stacktrace.parser-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [orchard.stacktrace.parser :as parser]
   [orchard.stacktrace.parser.test :as test]))

(deftest parse-test
  (doseq [fixture test/fixtures]
    (testing (format "parse fixture %s" fixture)
      (let [{:keys [cause error trace type] :as x} (parser/parse (test/read-fixture fixture))]
        (testing "should succeed"
          (is (nil? error)))
        (testing "should parse the cause"
          (is (string? cause)))
        (testing "should parse the trace"
          (is (every? test/stacktrace-element? trace)))))))

(deftest parse-input-transformation-test
  (doseq [fixture test/fixtures]
    (testing (format "parse fixture %s" fixture)
      (let [text (test/read-fixture fixture)]
        (testing "with input pr-str 1 level deep"
          (is (= (parser/parse text) (parser/parse (pr-str text)))))
        (testing "with input pr-str 2 levels deep"
          (is (= (parser/parse text) (parser/parse (pr-str (pr-str text))))))))))
