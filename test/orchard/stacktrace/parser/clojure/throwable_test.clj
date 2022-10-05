(ns orchard.stacktrace.parser.clojure.throwable-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [orchard.stacktrace.parser.clojure.throwable :as parser]
   [orchard.stacktrace.parser.test :as test]))

(def boom
  (ex-info "BOOM-1" {:boom "1"}
           (ex-info "BOOM-2" {:boom "2"}
                    (ex-info "BOOM-3" {:boom "3"}))))

(def divide-by-zero
  (try (/ 1 0) (catch Exception e e)))

(def short-boom
  (let [^Throwable exception (ex-info "BOOM-1" {:boom "1"})]
    (.setStackTrace exception (into-array [(last (.getStackTrace exception))]))
    exception))

(deftest parse-throwable-test
  (let [{:keys [cause data trace stacktrace-type via]} (parser/parse-stacktrace boom)]
    (testing ":stacktrace-type"
      (is (= :throwable stacktrace-type)))
    (testing "throwable cause"
      (is (= "BOOM-3" cause)))
    (testing ":data"
      (is (= {:boom "3"} data)))
    (testing ":via"
      (is (= 3 (count via)))
      (testing "first cause"
        (let [{:keys [at data message type]} (nth via 0)]
          (is (test/stacktrace-element? at))
          (is (= {:boom "1"} data))
          (is (= "BOOM-1" message))
          (is (= 'clojure.lang.ExceptionInfo type))))
      (testing "second cause"
        (let [{:keys [at data message type]} (nth via 1)]
          (is (test/stacktrace-element? at))
          (is (= {:boom "2"} data))
          (is (= "BOOM-2" message))
          (is (= 'clojure.lang.ExceptionInfo type))))
      (testing "third cause"
        (let [{:keys [at data message type]} (nth via 2)]
          (is (test/stacktrace-element? at))
          (is (= {:boom "3"} data))
          (is (= "BOOM-3" message))
          (is (= 'clojure.lang.ExceptionInfo type)))))
    (testing ":trace"
      (is (every? test/stacktrace-element? trace)))))

(deftest parse-stacktrace-divide-by-zero-test
  (let [{:keys [cause data trace stacktrace-type via]} (parser/parse-stacktrace divide-by-zero)]
    (testing ":stacktrace-type"
      (is (= :throwable stacktrace-type)))
    (testing "throwable cause"
      (is (= "Divide by zero" cause)))
    (testing ":data"
      (is (= nil data)))
    (testing ":via"
      (is (= 1 (count via)))
      (testing "first cause"
        (let [{:keys [at data message type]} (nth via 0)]
          (is (test/stacktrace-element? at))
          (is (= nil data))
          (is (= "Divide by zero" message))
          (is (= 'java.lang.ArithmeticException type)))))
    (testing ":trace"
      (is (every? test/stacktrace-element? trace)))))

(deftest parse-stacktrace-short-test
  (let [{:keys [cause data trace stacktrace-type via]} (parser/parse-stacktrace short-boom)]
    (testing ":stacktrace-type"
      (is (= :throwable stacktrace-type)))
    (testing "throwable cause"
      (is (= "BOOM-1" cause)))
    (testing ":data"
      (is (= {:boom "1"} data)))
    (testing ":via"
      (is (= 1 (count via)))
      (testing "first cause"
        (let [{:keys [at data message type]} (nth via 0)]
          (is (test/stacktrace-element? at))
          (is (= {:boom "1"} data))
          (is (= "BOOM-1" message))
          (is (= 'clojure.lang.ExceptionInfo type)))))
    (testing ":trace"
      (is (every? test/stacktrace-element? trace)))))

(deftest parse-stacktrace-error-test
  (testing "parsing a string not matching the grammar"
    (is (= {:error :unsupported
            :type :unsupported-input
            :input ""}
           (parser/parse-stacktrace "")))))
