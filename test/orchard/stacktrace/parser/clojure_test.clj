(ns orchard.stacktrace.parser.clojure-test
  (:require [clojure.test :refer [deftest is testing]]
            [orchard.stacktrace.parser.clojure :as parser]
            [orchard.stacktrace.parser.test :as test]))

(deftest parse-stacktrace-test
  (let [{:keys [cause data trace via product]} (test/parse-fixture :boom.clojure)]
    (testing "product"
      (is (= :clojure product)))
    (testing "throwable cause"
      (is (= "BOOM-3" cause)))
    (testing "throwable data"
      (is (= {:boom "3"} data)))
    (testing "throwable via"
      (is (= 3 (count via)))
      (testing "stacktrace first cause"
        (let [{:keys [at data message type]} (nth via 0)]
          (is (test/stacktrace-element? at))
          (is (= {:boom "1"} data))
          (is (= "BOOM-1" message))
          (is (= 'clojure.lang.ExceptionInfo type))))
      (testing "stacktrace second cause"
        (let [{:keys [at data message type]} (nth via 1)]
          (is (test/stacktrace-element? at))
          (is (= {:boom "2"} data))
          (is (= "BOOM-2" message))
          (is (= 'clojure.lang.ExceptionInfo type))))
      (testing "stacktrace third cause"
        (let [{:keys [at data message type]} (nth via 2)]
          (is (test/stacktrace-element? at))
          (is (= {:boom "3"} data))
          (is (= "BOOM-3" message))
          (is (= 'clojure.lang.ExceptionInfo type)))))
    (testing "throwable trace"
      (is (every? test/stacktrace-element? trace)))))

(deftest parse-stacktrace-divide-by-zero-test
  (let [{:keys [cause data trace via]} (test/parse-fixture :divide-by-zero.clojure)]
    (testing "throwable cause"
      (is (= "Divide by zero" cause)))
    (testing "throwable data"
      (is (= nil data)))
    (testing "throwable via"
      (is (= 1 (count via)))
      (testing "stacktrace first cause"
        (let [{:keys [at data message type]} (nth via 0)]
          (is (test/stacktrace-element? at))
          (is (= nil data))
          (is (= "Divide by zero" message))
          (is (= 'java.lang.ArithmeticException type)))))
    (testing "throwable trace"
      (is (every? test/stacktrace-element? trace)))))

(deftest parse-stacktrace-garbage-test
  (let [expected (test/read-fixture :boom.clojure)]
    (testing "parsing a stacktrace with garbage at the end should succeed"
      (is (= (parser/parse-stacktrace expected)
             (parser/parse-stacktrace (str expected "<garbage>")))))
    (testing "parsing a stacktrace with garbage at the end should succeed"
      (is (= (parser/parse-stacktrace expected)
             (parser/parse-stacktrace (str "<garbage>" expected)))))))

(deftest parse-stacktrace-incorrect-input-test
  (testing "parsing incorrect input"
    (let [{:keys [error input type]} (parser/parse-stacktrace "")]
      (testing "error"
        (is (= :incorrect error)))
      (testing "type"
        (is (= :incorrect-input type)))
      (testing "input"
        (is (= "" input))))))

(deftest parse-stacktrace-unsupported-input-test
  (testing "parsing unsupported input"
    (let [{:keys [error input type]} (parser/parse-stacktrace 1)]
      (testing "error"
        (is (= :unsupported error)))
      (testing "type"
        (is (= :input-not-supported type)))
      (testing "input"
        (is (= 1 input))))))
