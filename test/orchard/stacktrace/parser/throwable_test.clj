(ns orchard.stacktrace.parser.throwable-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [orchard.stacktrace.parser.test :as test]
   [orchard.stacktrace.parser.throwable :as parser]))

(def boom
  (ex-info "BOOM-1" {:boom "1"}
           (ex-info "BOOM-2" {:boom "2"}
                    (ex-info "BOOM-3" {:boom "3"}))))

(def divide-by-zero
  (try (/ 1 0) (catch Exception e e)))

(deftest parse-throwable-test
  (let [{:keys [cause data trace product via]} (parser/parse-stacktrace boom)]
    (testing "product"
      (is (= :throwable product)))
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
  (let [{:keys [cause data trace product via]} (parser/parse-stacktrace divide-by-zero)]
    (testing "product"
      (is (= :throwable product)))
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

(deftest parse-stacktrace-error-test
  (testing "parsing a string not matching the grammar"
    (is (= {:error :unsupported
            :type :input-not-supported
            :input ""}
           (parser/parse-stacktrace "")))))
