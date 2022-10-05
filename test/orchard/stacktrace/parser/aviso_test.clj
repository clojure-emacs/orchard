(ns orchard.stacktrace.parser.aviso-test
  (:require [clojure.test :refer [deftest is testing]]
            [orchard.stacktrace.parser.aviso :as parser]
            [orchard.stacktrace.parser.test :as test]))

(defn- parse-fixture [name]
  (some-> name test/read-fixture parser/parse-stacktrace))

(deftest parse-stacktrace-boom-test
  (let [{:keys [cause data trace stacktrace-type via]} (parse-fixture :boom.aviso)]
    (testing ":stacktrace-type"
      (is (= :aviso stacktrace-type)))
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

(deftest parse-stacktrace-boom-full-test
  (let [{:keys [cause data trace stacktrace-type via]} (parse-fixture :boom.aviso.full)]
    (testing ":stacktrace-type"
      (is (= :aviso stacktrace-type)))
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
        (is (= '[clojure.lang.AFn applyToHelper "AFn.java" 156] (first trace))))
      (testing "last frame"
        (is (= '[java.lang.Thread run "Thread.java" 829] (last trace)))))))

(deftest parse-stacktrace-divide-by-zero-test
  (let [{:keys [cause data trace stacktrace-type via]} (parse-fixture :divide-by-zero.aviso)]
    (testing ":stacktrace-type"
      (is (= :aviso stacktrace-type)))
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
  (let [{:keys [cause data trace stacktrace-type via]} (parse-fixture :short.aviso)]
    (testing ":stacktrace-type"
      (is (= :aviso stacktrace-type)))
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

(deftest parse-stacktrace-incorrect-input-test
  (testing "parsing a string not matching the grammar"
    (let [{:keys [error failure input type]} (parser/parse-stacktrace "")]
      (is (= :incorrect error))
      (is (= :incorrect-input type))
      (is (= "" input))
      (is (= {:index 0
              :reason
              [{:tag :regexp, :expecting "[a-zA-Z0-9_$*-]"}
               {:tag :regexp, :expecting "[^\\S\\r\\n]+"}]
              :line 1
              :column 1
              :text nil}
             (test/stringify-regexp failure))))))

(deftest parse-stacktrace-unsupported-input-test
  (testing "parsing unsupported input"
    (let [{:keys [error input type]} (parser/parse-stacktrace 1)]
      (is (= :unsupported error))
      (is (= :input-not-supported type))
      (is (= 1 input)))))
