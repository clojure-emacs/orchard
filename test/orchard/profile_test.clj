(ns orchard.profile-test
  (:require
   [clojure.test :as t :refer [deftest testing]]
   [matcher-combinators.matchers :as matchers]
   [orchard.profile :as sut]
   [orchard.test.util :refer [is+]]
   [orchard.trace-test.sample-ns :as sample-ns]))

(defn- run-fns []
  (dotimes [_ 10] (sample-ns/qux "abc" "efg")))

(deftest basic-profiling-test
  (sut/clear)
  (sut/profile-ns 'orchard.trace-test.sample-ns)
  (run-fns)

  (testing "summary returns profiling results for all vars"
    (is+ {#'sample-ns/baz {:name #'sample-ns/baz
                           :n 10
                           :mean number?
                           :std number?
                           :sum number?
                           :min number?
                           :max number?
                           :med number?
                           :samples vector?}
          #'sample-ns/bar {:name #'sample-ns/bar
                           :n 10
                           :mean number?
                           :std number?
                           :sum number?
                           :min number?
                           :max number?
                           :med number?
                           :samples vector?}
          #'sample-ns/foo map?
          #'sample-ns/qux map?}
         (sut/summary)))

  (sut/clear)
  (sut/unprofile-var #'sample-ns/foo)
  (sut/unprofile-var #'sample-ns/qux)
  (run-fns)

  (testing "only two vars are profiled now"
    (is+ {#'sample-ns/baz map?
          #'sample-ns/bar map?
          #'sample-ns/foo matchers/absent
          #'sample-ns/qux matchers/absent}
         (sut/summary)))

  (sut/clear)
  (sut/unprofile-var #'sample-ns/bar)
  (sut/unprofile-var #'sample-ns/baz)
  (run-fns)
  (testing "no vars are profiled now"
    (is+ empty? (sut/summary)))

  (sut/profile-ns 'orchard.trace-test.sample-ns)
  (sut/unprofile-ns 'orchard.trace-test.sample-ns)
  (run-fns)
  (testing "turning namespace profiling on and then off leaves no vars profiled"
    (is+ empty? (sut/summary))))

(deftest too-many-samples-test
  (sut/clear)
  (sut/profile-ns 'orchard.trace-test.sample-ns)
  (dotimes [_ 1e6] (sample-ns/qux "abc" "efg"))
  (sut/summary)
  (testing "overflow samples are still counted"
    (is+ 1000000 (:n (get (sut/summary) #'sample-ns/qux)))))

(deftest summary-for-inspector-test
  (sut/clear)
  (sut/profile-ns 'orchard.trace-test.sample-ns)
  (run-fns)
  (is+ [{:name #'sample-ns/bar
         :n 10
         :mean (matchers/via str #" [num]?s$")
         :std (matchers/via str #"^±.+ [num]?s$")
         :sum (matchers/via str #" [num]?s$")
         :min (matchers/via str #" [num]?s$")
         :max (matchers/via str #" [num]?s$")
         :med (matchers/via str #" [num]?s$")}
        {:name #'sample-ns/baz, :n 10}
        {:name #'sample-ns/foo, :n 10}
        {:name #'sample-ns/qux, :n 10}]
       (sut/summary-for-inspector)))
