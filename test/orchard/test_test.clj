(ns orchard.test-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [matcher-combinators.matchers :as matchers]
   [orchard.test :as sut]
   [orchard.test-fixtures.hook]
   [orchard.test-fixtures.hook-and-vars]
   [orchard.test-fixtures.mixed]
   [orchard.test-fixtures.throwing-fixture]
   [orchard.test-fixtures.throwing-hook]
   [orchard.test.util :refer [is+]])
  (:import
   (clojure.lang ExceptionInfo)))

;; Running tests from within tests is fine as long as the outer runner isn't
;; `orchard.test` itself: the inner run rebinds `clojure.test/report` and the
;; report counters only for its own duration.

(def mixed 'orchard.test-fixtures.mixed)

(defn- run-mixed
  ([] (run-mixed {}))
  ([opts]
   (sut/run-var-query {:ns-query {:exactly [mixed]}} opts)))

(deftest run-var-query-test
  (let [report (run-mixed)]
    (is+ {:summary {:ns 1 :var 4 :test 5 :pass 3 :fail 1 :error 1}
          :results {mixed {'passing [{:type :pass}]
                           'failing [{:type :fail
                                      :context "a context"
                                      :expected "(= {:a 1} {:a 2})\n"
                                      :actual string?}]
                           'erroring [{:type :error
                                       :fault true
                                       :error #(instance? ExceptionInfo %)
                                       :line some?}]
                           'two-assertions [{:type :pass :index 0}
                                            {:type :pass :index 1}]}}
          :elapsed-time {:ms int?}
          :ns-elapsed-time {mixed {:ms int?}}}
         report)
    (testing "a var with a single assertion carries that assertion's timing"
      (is+ {:elapsed-time {:ms int?}}
           (-> report :results (get mixed) (get 'passing) first)))
    (testing "every var gets its own timing"
      (is (= #{'passing 'failing 'erroring 'two-assertions}
             (set (keys (get-in report [:var-elapsed-time mixed])))))))
  (testing "an enclosing `testing` doesn't end up in the results' context"
    (is (= "a context"
           (-> (run-mixed) :results (get mixed) (get 'failing) first :context)))))

(deftest filtering-test
  (is (= ['passing]
         (-> (sut/run-var-query {:ns-query {:exactly [mixed]}
                                 :include-meta-key [:smoke]})
             :results (get mixed) keys)))
  (testing "picking vars leaves out the test-ns-hook namespaces"
    (is+ {:summary {:ns 1 :var 1}
          :results (matchers/equals {mixed {'passing some?}})}
         (sut/run-var-query {:exactly [#'orchard.test-fixtures.mixed/passing]})))
  (testing "namespaces without tests aren't counted"
    (is+ {:summary {:ns 1}}
         (sut/run-var-query {:ns-query {:exactly [mixed 'clojure.set]}}))))

(deftest run-namespaces-test
  (is+ {:summary {:var 2 :pass 3}
        :results (matchers/equals {mixed {'passing some?
                                          'two-assertions some?}})}
       (sut/run-namespaces {mixed ['passing 'two-assertions]}))
  (testing "nil runs all of a namespace's tests, and only those"
    (reset! orchard.test-fixtures.mixed/each-fixture-calls 0)
    (is+ {:summary {:ns 1 :var 4 :test 5}}
         (sut/run-namespaces {mixed nil}))
    (is (= 4 @orchard.test-fixtures.mixed/each-fixture-calls)))
  (testing "namespaces without tests are left out"
    (is+ {:summary {:ns 0}}
         (sut/run-namespaces {'clojure.set nil})))
  (testing "results with no var to run again are skipped"
    (is+ {:summary {:ns 2 :var 1 :pass 2}}
         (sut/run-namespaces {'orchard.test-fixtures.hook [sut/fallback-var-name]
                              mixed [sut/fallback-var-name 'passing]}))))

(deftest fail-fast-test
  (testing "the run stops after the first failing var"
    (let [report (run-mixed {:fail-fast? true})]
      (is (= 1 (+ (-> report :summary :fail)
                  (-> report :summary :error)))))))

(deftest throwing-fixture-test
  (is+ {:summary {:ns 1 :test 0 :error 1}
        :results {'orchard.test-fixtures.throwing-fixture
                  {'throwing-fixture [{:type :error
                                       :message "Uncaught exception in test fixture"}]}}}
       (sut/run-var-query {:ns-query {:exactly ['orchard.test-fixtures.throwing-fixture]}})))

(deftest test-ns-hook-test
  (is+ {:summary {:test 1 :pass 1 :fail 0 :error 0}}
       (sut/run-var-query {:ns-query {:exactly ['orchard.test-fixtures.hook]
                                      :has-tests? true}}))
  (testing "a namespace with both test vars and a hook runs once"
    (is+ {:summary {:ns 1 :var 2 :test 2 :pass 2}}
         (sut/run-var-query {:ns-query {:exactly ['orchard.test-fixtures.hook-and-vars]}})))
  (testing "a throwing hook is reported as an error and the run goes on"
    (is+ {:summary {:ns 2 :error 1}
          :results {'orchard.test-fixtures.throwing-hook
                    {'test-ns-hook [{:type :error
                                     :message "Uncaught exception in test-ns-hook"
                                     :error #(instance? ExceptionInfo %)
                                     :line int?}]}
                    mixed {'passing [{:type :pass}]}}}
         (sut/run-namespaces {'orchard.test-fixtures.throwing-hook nil
                              mixed ['passing]}))))

(deftest on-event-test
  (let [events (atom [])
        report (run-mixed {:on-event #(swap! events conj %)})]
    (testing "a namespace is announced before its vars and closed after them"
      (is (= :begin-ns (:type (first @events))))
      (is+ {:type :end-ns :ns mixed :elapsed-time {:ms int?}}
           (last @events)))
    (testing "every var reports its results once, including the one that threw"
      (is (= #{'passing 'failing 'erroring 'two-assertions}
             (->> @events (filter (comp #{:end-var} :type)) (map :var) set)))
      (is+ {:type :end-var
            :ns mixed
            :var 'failing
            :results [{:type :fail :expected string?}]}
           (->> @events (filter (comp #{'failing} :var)) first)))
    (testing "the running summary ends up equal to the final one"
      (is (= (:summary report)
             (->> @events (filter (comp #{:end-var} :type)) last :summary)))))

  (testing "a failing fixture still produces an event"
    (let [events (atom [])]
      (sut/run-var-query {:ns-query {:exactly ['orchard.test-fixtures.throwing-fixture]}}
                         {:on-event #(swap! events conj %)})
      (is+ [{:type :begin-ns}
            {:type :end-var :results [{:type :error}]}
            {:type :end-ns}]
           @events)))

  (testing "a test-ns-hook namespace reports its vars"
    (let [events (atom [])]
      (sut/run-var-query {:ns-query {:exactly ['orchard.test-fixtures.hook]
                                     :has-tests? true}}
                         {:on-event #(swap! events conj %)})
      (is+ [{:type :begin-ns} {:type :end-ns}]
           [(first @events) (last @events)])))

  (testing "a throwing test-ns-hook still produces an event"
    (let [events (atom [])]
      (sut/run-namespaces {'orchard.test-fixtures.throwing-hook nil}
                          {:on-event #(swap! events conj %)})
      (is+ [{:type :begin-ns}
            {:type :end-var :var 'test-ns-hook :results [{:type :error}]}
            {:type :end-ns}]
           @events))))

(deftest non-fixture-throwable-test
  ;; A throwable that escapes a test body with no fixture frame in its trace
  ;; (e.g. interrupting a test that isn't wrapped in `is`) is reported as an
  ;; error, instead of crashing on the missing fixture.
  (reset! sut/current-report {:summary {:test 1 :error 0} :results {}
                              :testing-ns 'orchard.test-test})
  (let [e (doto (InterruptedException. "interrupted")
            (.setStackTrace (make-array StackTraceElement 0)))]
    (sut/report-fixture-error (the-ns 'orchard.test-test) e)
    (is (= 1 (get-in @sut/current-report [:summary :error])))))

(deftest callback-failure-test
  (testing "a throwing :on-event doesn't change the outcome"
    (is (= (:summary (run-mixed))
           (:summary (run-mixed {:on-event (fn [_] (throw (Exception. "gone")))}))))))

(deftest print-object-test
  (testing "maps are printed with sorted keys"
    (is (= "{:a 1, :b 2, :c 3, :d {x 1, y 2, z 3}}\n"
           (sut/print-object {:b 2 :c 3 :a 1 :d {'z 3 'y 2 'x 1}}))))
  (testing "pprint is used"
    (is (= "{:a\n (\"a-sufficiently-long-string\"\n  \"a-sufficiently-long-string\"\n  \"a-sufficiently-long-string\")}\n"
           (sut/print-object {:a (repeat 3 "a-sufficiently-long-string")})))))

(deftest test-error-handler-test
  (let [proof (atom [])
        exception (ex-info "." {::unique (rand)})]
    (binding [sut/*test-error-handler* #(swap! proof conj %)]
      (sut/test-result 'some-ns #'+ {:type :error :actual exception}))
    (is (= [exception] @proof))))

(defn throws []
  (throw (ex-info "." {})))

(deftest stack-frame-test
  (let [e (try
            (throws)
            (catch ExceptionInfo e
              e))]
    (is+ {:fn "throws"
          :ns "orchard.test-test"
          :var "orchard.test-test/throws"
          :file "test_test.clj"}
         (sut/stack-frame e throws))))
