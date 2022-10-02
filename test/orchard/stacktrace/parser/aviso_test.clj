(ns orchard.stacktrace.parser.aviso-test
  (:require [clojure.test :refer [deftest is testing]]
            [orchard.stacktrace.parser.aviso :as parser]
            [orchard.stacktrace.parser.test :as test]))

(defn- parse-fixture [name]
  (some-> name test/read-fixture parser/parse-stacktrace))

(deftest parse-stacktrace-boom-test
  (let [{:keys [cause data trace product via]} (parse-fixture :boom.aviso)]
    (testing ":product"
      (is (= :aviso product)))
    (testing "throwable cause"
      (is (= "BOOM-3" cause)))
    (testing ":data"
      (is (= {:boom "3"} data)))
    (testing ":via"
      (is (= 3 (count via)))
      (testing "first cause"
        (let [{:keys [at data message type]} (nth via 0)]
          (is (nil? at))
          (is (= {:boom "1"} data))
          (is (= "BOOM-1" message))
          (is (= 'clojure.lang.ExceptionInfo type))))
      (testing "second cause"
        (let [{:keys [at data message type]} (nth via 1)]
          (is (nil? at))
          (is (= {:boom "2"} data))
          (is (= "BOOM-2" message))
          (is (= 'clojure.lang.ExceptionInfo type))))
      (testing "third cause"
        (let [{:keys [at data message type]} (nth via 2)]
          (is (nil? at))
          (is (= {:boom "3"} data))
          (is (= "BOOM-3" message))
          (is (= 'clojure.lang.ExceptionInfo type)))))
    (testing ":trace"
      (is (every? test/stacktrace-element? trace))
      (testing "first frame"
        (is (= '[orchard.stacktrace.parser.throwable-test eval12321 "REPL Input"] (first trace))))
      (testing "last frame"
        (is (= '[nrepl.middleware.interruptible-eval evaluate/fn "interruptible_eval.clj" 87] (last trace)))))))

(deftest parse-stacktrace-divide-by-zero-test
  (let [{:keys [cause data trace product via]} (parse-fixture :divide-by-zero.aviso)]
    (testing ":product"
      (is (= :aviso product)))
    (testing "throwable cause"
      (is (= "Divide by zero" cause)))
    (testing ":data"
      (is (= nil data)))
    (testing ":via"
      (is (= 1 (count via)))
      (testing "first cause"
        (let [{:keys [at data message type]} (nth via 0)]
          (is (nil? at))
          (is (= nil data))
          (is (= "Divide by zero" message))
          (is (= 'java.lang.ArithmeticException type)))))
    (testing ":trace"
      (is (every? test/stacktrace-element? trace))
      (testing "first frame"
        (is (= '[orchard.stacktrace.parser.throwable-test fn "throwable_test.clj" 13]
               (first trace))))
      (testing "last frame"
        (is (= '[nrepl.middleware.interruptible-eval evaluate/fn "interruptible_eval.clj" 87]
               (last trace)))))))

(deftest parse-stacktrace-short-test
  (let [{:keys [cause data trace product via]} (parse-fixture :short.aviso)]
    (testing ":product"
      (is (= :aviso product)))
    (testing "throwable cause"
      (is (= "BOOM-1" cause)))
    (testing ":data"
      (is (= {:boom "1"} data)))
    (testing ":via"
      (is (= 1 (count via)))
      (testing "first cause"
        (let [{:keys [at data message type]} (nth via 0)]
          (is (nil? at))
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
  (let [text (test/read-fixture :boom.aviso)
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
