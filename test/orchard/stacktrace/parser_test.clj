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

(deftest parse-garbage-test
  (doseq [fixture test/fixtures]
    (testing (format "parse fixture %s with" fixture)
      (let [text (test/read-fixture fixture)
            expected (parser/parse text)]
        (testing "garbage at the beginning"
          (is (= expected (parser/parse (str "\n<garbage>\n<garbage>\n" text)))))
        (testing "garbage at the end"
          (is (= expected (parser/parse (str text "\n<garbage>\n<garbage>\n")))))
        (testing "white space in front"
          (is (= expected (parser/parse (str " \t " text)))))
        (testing "white space at the end"
          (is (= expected (parser/parse (str text " \t ")))))
        (testing "newlines in front"
          (is (= expected (parser/parse (str text "\n\n")))))
        (testing "newlines at the end"
          (is (= expected (parser/parse (str text "\n\n")))))))))

(deftest parse-input-transformation-test
  (doseq [fixture test/fixtures]
    (testing (format "parse fixture %s" fixture)
      (let [text (test/read-fixture fixture)]
        (testing "with input pr-str 1 level deep"
          (is (= (parser/parse text) (parser/parse (pr-str text)))))
        (testing "with input pr-str 2 levels deep"
          (is (= (parser/parse text) (parser/parse (pr-str (pr-str text))))))))))
