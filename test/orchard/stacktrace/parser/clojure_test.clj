(ns orchard.stacktrace.parser.clojure-test
  (:require [clojure.test :refer [deftest is testing]]
            [orchard.stacktrace.parser.clojure :as parser]
            [orchard.stacktrace.parser.test :as test]))

(defn- parse-fixture [name]
  (some-> name test/read-fixture parser/parse-stacktrace))

(deftest parse-throwable-test
  (let [{:keys [cause data trace product via]} (parse-fixture :boom.clojure)]
    (testing ":product"
      (is (= :clojure product)))
    (testing "throwable cause"
      (is (= "BOOM-3" cause)))
    (testing ":data"
      (is (= {:boom "3"} data)))
    (testing ":via"
      (is (= 3 (count via)))
      (testing "first cause"
        (let [{:keys [at data message type]} (nth via 0)]
          (is (= '[clojure.lang.AFn applyToHelper "AFn.java" 160] at))
          (is (= {:boom "1"} data))
          (is (= "BOOM-1" message))
          (is (= 'clojure.lang.ExceptionInfo type))))
      (testing "second cause"
        (let [{:keys [at data message type]} (nth via 1)]
          (is (= '[clojure.lang.AFn applyToHelper "AFn.java" 160] at))
          (is (= {:boom "2"} data))
          (is (= "BOOM-2" message))
          (is (= 'clojure.lang.ExceptionInfo type))))
      (testing "third cause"
        (let [{:keys [at data message type]} (nth via 2)]
          (is (= '[clojure.lang.AFn applyToHelper "AFn.java" 156] at))
          (is (= {:boom "3"} data))
          (is (= "BOOM-3" message))
          (is (= 'clojure.lang.ExceptionInfo type)))))
    (testing ":trace"
      (is (every? test/stacktrace-element? trace))
      (testing "first frame"
        (is (= '[clojure.lang.AFn applyToHelper "AFn.java" 156] (first trace))))
      (testing "last frame"
        (is (= '[java.lang.Thread run "Thread.java" 829] (last trace)))))))

(deftest parse-stacktrace-divide-by-zero-test
  (let [{:keys [cause data trace product via]} (parse-fixture :divide-by-zero.clojure)]
    (testing ":product"
      (is (= :clojure product)))
    (testing "throwable cause"
      (is (= "Divide by zero" cause)))
    (testing ":data"
      (is (= nil data)))
    (testing ":via"
      (is (= 1 (count via)))
      (testing "first cause"
        (let [{:keys [at data message type]} (nth via 0)]
          (is (= '[clojure.lang.Numbers divide "Numbers.java" 188] at))
          (is (= nil data))
          (is (= "Divide by zero" message))
          (is (= 'java.lang.ArithmeticException type)))))
    (testing ":trace"
      (is (every? test/stacktrace-element? trace))
      (testing "first frame"
        (is (= '[clojure.lang.Numbers divide "Numbers.java" 188] (first trace))))
      (testing "last frame"
        (is (= '[java.lang.Thread run "Thread.java" 829] (last trace)))))))

(deftest parse-stacktrace-short-test
  (let [{:keys [cause data trace product via]} (parse-fixture :short.clojure)]
    (testing ":product"
      (is (= :clojure product)))
    (testing "throwable cause"
      (is (= "BOOM-1" cause)))
    (testing ":data"
      (is (= {:boom "1"} data)))
    (testing ":via"
      (is (= 1 (count via)))
      (testing "first cause"
        (let [{:keys [at data message type]} (nth via 0)]
          (is (= '[java.lang.Thread run "Thread.java" 829] at))
          (is (= {:boom "1"} data))
          (is (= "BOOM-1" message))
          (is (= 'clojure.lang.ExceptionInfo type)))))
    (testing ":trace"
      (is (every? test/stacktrace-element? trace))
      (testing "first frame"
        (is (= '[java.lang.Thread run "Thread.java" 829] (first trace))))
      (testing "last frame"
        (is (= '[java.lang.Thread run "Thread.java" 829] (last trace)))))))

(deftest parse-stacktrace-garbage-test
  (let [expected (test/read-fixture :boom.clojure)]
    (testing "parsing a stacktrace with garbage at the end should succeed"
      (is (= (parser/parse-stacktrace expected)
             (parser/parse-stacktrace (str expected "<garbage>"))
             (parser/parse-stacktrace (str expected "\n<garbage>\n")))))
    (testing "parsing a stacktrace with garbage at the end should succeed"
      (is (= (parser/parse-stacktrace expected)
             (parser/parse-stacktrace (str "<garbage>" expected))
             (parser/parse-stacktrace (str "\n<garbage>\n" expected)))))))

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
