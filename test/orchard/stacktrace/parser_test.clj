(ns orchard.stacktrace.parser-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [orchard.stacktrace.parser :as parser]))

(defn- stacktrace-element? [element]
  (let [[class method file line] element]
    (and (symbol? class)
         (symbol? method)
         (string? file)
         (number? line))))

(def boom-tagged-literal-str
  (slurp (io/resource "orchard/stacktrace/parser/boom.tagged-literal.txt")))

(deftest parse-tagged-literal-test
  (let [{:keys [cause data trace via]} (parser/parse boom-tagged-literal-str)]
    (testing "throwable cause"
      (is (= "BOOM-3" cause)))
    (testing "throwable data"
      (is (= {:boom "3"} data)))
    (testing "throwable via"
      (is (= 3 (count via)))
      (testing "stacktrace first cause"
        (let [{:keys [at data message type]} (nth via 0)]
          (is (stacktrace-element? at))
          (is (= {:boom "1"} data))
          (is (= "BOOM-1" message))
          (is (= 'clojure.lang.ExceptionInfo type))))
      (testing "stacktrace second cause"
        (let [{:keys [at data message type]} (nth via 1)]
          (is (stacktrace-element? at))
          (is (= {:boom "2"} data))
          (is (= "BOOM-2" message))
          (is (= 'clojure.lang.ExceptionInfo type))))
      (testing "stacktrace third cause"
        (let [{:keys [at data message type]} (nth via 2)]
          (is (stacktrace-element? at))
          (is (= {:boom "3"} data))
          (is (= "BOOM-3" message))
          (is (= 'clojure.lang.ExceptionInfo type)))))
    (testing "throwable trace"
      (is (every? stacktrace-element? trace)))))

(deftest parse-tagged-literal-buried-test
  (testing "parse printed exception buried in a string"
    (is (= (parser/parse boom-tagged-literal-str)
           (parser/parse (pr-str boom-tagged-literal-str))
           (parser/parse (pr-str (pr-str boom-tagged-literal-str)))))))

(deftest parse-throwable-test
  (let [{:keys [cause data trace via]}
        (parser/parse (ex-info "BOOM-1" {:boom "1"}
                               (ex-info "BOOM-2" {:boom "2"}
                                        (ex-info "BOOM-3" {:boom "3"}))))]
    (testing "throwable cause"
      (is (= "BOOM-3" cause)))
    (testing "throwable data"
      (is (= {:boom "3"} data)))
    (testing "throwable via"
      (is (= 3 (count via)))
      (testing "stacktrace first cause"
        (let [{:keys [at data message type]} (nth via 0)]
          (is (stacktrace-element? at))
          (is (= {:boom "1"} data))
          (is (= "BOOM-1" message))
          (is (= 'clojure.lang.ExceptionInfo type))))
      (testing "stacktrace second cause"
        (let [{:keys [at data message type]} (nth via 1)]
          (is (stacktrace-element? at))
          (is (= {:boom "2"} data))
          (is (= "BOOM-2" message))
          (is (= 'clojure.lang.ExceptionInfo type))))
      (testing "stacktrace third cause"
        (let [{:keys [at data message type]} (nth via 2)]
          (is (stacktrace-element? at))
          (is (= {:boom "3"} data))
          (is (= "BOOM-3" message))
          (is (= 'clojure.lang.ExceptionInfo type)))))
    (testing "throwable trace"
      (is (every? stacktrace-element? trace)))))
