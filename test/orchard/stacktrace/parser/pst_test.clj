(ns orchard.stacktrace.parser.pst-test
  (:require [clojure.test :refer [deftest is testing]]
            [orchard.stacktrace.parser.pst :as parser]
            [orchard.stacktrace.parser.test :as test]))

(defn- parse-fixture [name]
  (some-> name test/read-fixture parser/parse-stacktrace))

(deftest parse-throwable-test
  (let [{:keys [cause data trace stacktrace-type via]} (parse-fixture :boom.pst)]
    (testing ":stacktrace-type"
      (is (= :pst stacktrace-type)))
    (testing "throwable cause"
      (is (= "BOOM-3" cause)))
    (testing ":data"
      (is (= {:boom "3"} data)))
    (testing ":via"
      (is (= 3 (count via)))
      (testing "first cause"
        (let [{:keys [at data message trace type]} (nth via 0)]
          (is (= '[clojure.lang.Compiler$InvokeExpr eval "Compiler.java" 3706] at))
          (is (= {:boom "1"} data))
          (is (= "BOOM-1" message))
          (is (= 'ExceptionInfo type))
          (is (= '[[clojure.lang.Compiler$InvokeExpr eval "Compiler.java" 3706]
                   [clojure.lang.Compiler$DefExpr eval "Compiler.java" 457]
                   [clojure.lang.Compiler eval "Compiler.java" 7186]]
                 (take 3 trace)))))
      (testing "second cause"
        (let [{:keys [at data message trace type]} (nth via 1)]
          (is (= '[clojure.lang.Compiler$InvokeExpr eval "Compiler.java" 3706] at))
          (is (= {:boom "2"} data))
          (is (= "BOOM-2" message))
          (is (= 'ExceptionInfo type))
          (is (= '[[clojure.lang.Compiler$InvokeExpr eval "Compiler.java" 3706]
                   [clojure.lang.Compiler$InvokeExpr eval "Compiler.java" 3705]
                   [clojure.lang.Compiler$DefExpr eval "Compiler.java" 457]]
                 (take 3 trace)))))
      (testing "third cause"
        (let [{:keys [at data message trace type]} (nth via 2)]
          (is (= '[clojure.lang.Compiler$InvokeExpr eval "Compiler.java" 3706] at))
          (is (= {:boom "3"} data))
          (is (= "BOOM-3" message))
          (is (= 'ExceptionInfo type))
          (is (= '[[clojure.lang.Compiler$InvokeExpr eval "Compiler.java" 3706]
                   [clojure.lang.Compiler$InvokeExpr eval "Compiler.java" 3705]
                   [clojure.lang.Compiler$InvokeExpr eval "Compiler.java" 3705]]
                 (take 3 trace))))))
    (testing ":trace"
      (is (every? test/stacktrace-element? trace))
      (testing "first frame"
        (is (= '[clojure.lang.Compiler$InvokeExpr eval "Compiler.java" 3706] (first trace))))
      (testing "last frame"
        (is (= '[clojure.lang.Compiler$InvokeExpr eval "Compiler.java" 3705] (last trace)))))))

(deftest parse-stacktrace-divide-by-zero-test
  (let [{:keys [cause data trace stacktrace-type via]} (parse-fixture :divide-by-zero.pst)]
    (testing ":stacktrace-type"
      (is (= :pst stacktrace-type)))
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
  (let [{:keys [cause data trace stacktrace-type via]} (parse-fixture :short.pst)]
    (testing ":stacktrace-type"
      (is (= :pst stacktrace-type)))
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
