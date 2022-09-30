(ns orchard.stacktrace.parser.java-test
  (:require [clojure.test :refer [deftest is testing]]
            [orchard.stacktrace.parser.java :as parser]
            [orchard.stacktrace.parser.test :as test]))

(defn- parse-fixture [name]
  (some-> name test/read-fixture parser/parse-stacktrace))

(deftest parse-stacktrace-test
  (let [{:keys [cause data trace product via]} (parse-fixture :boom.java)]
    (testing "product"
      (is (= :java product)))
    (testing ":cause"
      (is (= "BOOM-3" cause)))
    (testing ":data"
      (is (= {:boom "3"} data)))
    (testing ":via"
      (is (= '[{:at [clojure.lang.AFn applyToHelper "AFn.java" 160]
                :data {:boom "1"}
                :message "BOOM-1"
                :type clojure.lang.ExceptionInfo}
               {:at [clojure.lang.AFn applyToHelper "AFn.java" 160]
                :data {:boom "2"}
                :message "BOOM-2"
                :type clojure.lang.ExceptionInfo}
               {:at [clojure.lang.AFn applyToHelper "AFn.java" 156]
                :data {:boom "3"}
                :message "BOOM-3"
                :type clojure.lang.ExceptionInfo}]
             via)))
    (testing ":trace"
      (is (= '[[clojure.lang.AFn applyToHelper "AFn.java" 160]
               [clojure.lang.AFn applyTo "AFn.java" 144]
               [clojure.lang.Compiler$InvokeExpr eval "Compiler.java" 3706]]
             (take 3 trace))))))

(deftest parse-stacktrace-divide-by-zero-test
  (let [{:keys [cause data trace via]} (parse-fixture :divide-by-zero.java)]
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

(deftest parse-stacktrace-short-test
  (let [{:keys [cause data trace product via]} (parse-fixture :short.java)]
    (testing "product"
      (is (= :java product)))
    (testing "throwable cause"
      (is (= "BOOM-1" cause)))
    (testing "throwable data"
      (is (= {:boom "1"} data)))
    (testing "throwable via"
      (is (= 1 (count via)))
      (testing "stacktrace first cause"
        (let [{:keys [at data message type]} (nth via 0)]
          (is (test/stacktrace-element? at))
          (is (= {:boom "1"} data))
          (is (= "BOOM-1" message))
          (is (= 'clojure.lang.ExceptionInfo type)))))
    (testing "throwable trace"
      (is (= 1 (count trace)))
      (is (every? test/stacktrace-element? trace))
      (testing "stacktrace first frame"
        (let [[class method file line] (first trace)]
          (is (= 'orchard.stacktrace.parser.throwable_test$eval16655 class))
          (is (= 'invokeStatic method))
          (is (= "form-init17983781294375166615.clj" file))
          (is (= 74 line)))))))

(deftest parse-stacktrace-garbage-test
  (let [text (test/read-fixture :boom.java)
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
      (testing "error"
        (is (= :incorrect error)))
      (testing "type"
        (is (= :incorrect-input type)))
      (testing "input"
        (is (= "" input)))
      (testing "failure"
        (is (= {:index 0
                :reason
                [{:tag :regexp :expecting "[a-zA-Z0-9_$/-]+"}
                 {:tag :regexp :expecting "[^\\S\\r\\n]+"}]
                :line 1
                :column 1
                :text nil}
               (test/stringify-regexp failure)))))))

(deftest parse-stacktrace-unsupported-input-test
  (testing "parsing unsupported input"
    (let [{:keys [error input type]} (parser/parse-stacktrace 1)]
      (testing "error"
        (is (= :unsupported error)))
      (testing "type"
        (is (= :input-not-supported type)))
      (testing "input"
        (is (= 1 input))))))
