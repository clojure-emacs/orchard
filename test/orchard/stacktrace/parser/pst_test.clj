(ns orchard.stacktrace.parser.pst-test
  (:require [clojure.test :refer [deftest is testing]]
            [orchard.stacktrace.parser.pst :as parser]
            [orchard.stacktrace.parser.test :as test]))

(defn- parse-fixture [name]
  (some-> name test/read-fixture parser/parse-stacktrace))

(deftest parse-throwable-test
  (let [{:keys [cause data trace product via]} (parse-fixture :boom.pst)]
    (testing ":product"
      (is (= :pst product)))
    (testing "throwable cause"
      (is (= "BOOM-3" cause)))
    (testing ":data"
      (is (= {:boom "3"} data)))
    (testing ":via"
      (is (= 3 (count via)))
      (testing "first cause"
        (let [{:keys [at data message type]} (nth via 0)]
          (is (= '[clojure.lang.Compiler$InvokeExpr eval "Compiler.java" 3706] at))
          (is (= {:boom "1"} data))
          (is (= "BOOM-1" message))
          (is (= 'ExceptionInfo type))))
      (testing "second cause"
        (let [{:keys [at data message type]} (nth via 1)]
          (is (= '[clojure.lang.Compiler$InvokeExpr eval "Compiler.java" 3706] at))
          (is (= {:boom "2"} data))
          (is (= "BOOM-2" message))
          (is (= 'ExceptionInfo type))))
      (testing "third cause"
        (let [{:keys [at data message type]} (nth via 2)]
          (is (= '[clojure.lang.Compiler$InvokeExpr eval "Compiler.java" 3706] at))
          (is (= {:boom "3"} data))
          (is (= "BOOM-3" message))
          (is (= 'ExceptionInfo type)))))
    (testing ":trace"
      (is (every? test/stacktrace-element? trace))
      (testing "first frame"
        (is (= '[clojure.lang.Compiler$InvokeExpr eval "Compiler.java" 3706] (first trace))))
      (testing "last frame"
        (is (= '[clojure core/apply "core.clj" 667] (last trace)))))))

(deftest parse-stacktrace-divide-by-zero-test
  (let [{:keys [cause data trace product via]} (parse-fixture :divide-by-zero.pst)]
    (testing ":product"
      (is (= :pst product)))
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
          (is (= 'ArithmeticException type)))))
    (testing ":trace"
      (is (every? test/stacktrace-element? trace))
      (testing "first frame"
        (is (= '[clojure.lang.Numbers divide "Numbers.java" 188] (first trace))))
      (testing "last frame"
        (is (= '[clojure.lang.Compiler eval "Compiler.java" 7136] (last trace)))))))

(deftest parse-stacktrace-short-test
  (let [{:keys [cause data trace product via]} (parse-fixture :short.pst)]
    (testing ":product"
      (is (= :pst product)))
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
          (is (= 'ExceptionInfo type)))))
    (testing ":trace"
      (is (every? test/stacktrace-element? trace))
      (testing "first frame"
        (is (= '[java.lang.Thread run "Thread.java" 829] (first trace))))
      (testing "last frame"
        (is (= '[java.lang.Thread run "Thread.java" 829] (last trace)))))))

(deftest parse-stacktrace-garbage-test
  (let [text (test/read-fixture :boom.pst)
        expected (parser/parse-stacktrace text)]
    (testing "garbage at the beginning is ignored"
      (is (= expected (parser/parse-stacktrace (str "\n<garbage>\n<garbage>\n" text)))))
    (testing "garbage at the end is ignored"
      (is (= expected (parser/parse-stacktrace (str text "\n<garbage>\n<garbage>\n")))))
    (testing "white space in front of exception is ignored"
      (is (= expected (parser/parse-stacktrace (str " \t " text)))))
    (testing "white space at the end of the exception is ignored"
      (is (= expected (parser/parse-stacktrace (str text " \t ")))))
    (testing "newlines at in front of the exception is ignored"
      (is (= expected (parser/parse-stacktrace (str text "\n\n")))))
    (testing "newlines at the end of the exception is ignored"
      (is (= expected (parser/parse-stacktrace (str text "\n\n")))))))

(deftest parse-stacktrace-incorrect-input-test
  (testing "parsing a string not matching the grammar"
    (let [{:keys [error failure input type]} (parser/parse-stacktrace "")]
      (is (= :incorrect error))
      (is (= :incorrect-input type))
      (is (= "" input))
      (is (nil? failure)))))

(deftest parse-stacktrace-unsupported-input-test
  (testing "parsing unsupported input"
    (let [{:keys [error input type]} (parser/parse-stacktrace 1)]
      (is (= :unsupported error))
      (is (= :input-not-supported type))
      (is (= 1 input)))))
