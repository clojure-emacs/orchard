(ns orchard.trace-test
  (:require
   [clojure.edn]
   [clojure.string :as str]
   [clojure.test :as t :refer [is are deftest testing]]
   [orchard.test.util :refer [with-out-str-rn]]
   [orchard.trace :as sut]
   [orchard.trace-test.sample-ns :as sample-ns]))

(deftest funcall-to-string-test
  (are [args expected] (= expected (#'sut/funcall-to-string "foo" args "|||  "))
    []                                   "|||  (foo)"
    (map str (range 10 20))              "|||  (foo 10 11 12 13 14 15 16 17 18 19)"
    (map str (range 10 30))              "|||  (foo 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29)"
    ["bar" "baz" (apply str (range 20))] "|||  (foo bar baz 012345678910111213141516171819)"
    ["bar" "baz" (apply str (range 25))] "|||  (foo bar baz 0123456789101112131415161718192021222324)"
    [(apply str (range 25)) "bar" "baz"] "|||  (foo 0123456789101112131415161718192021222324 bar baz)"
    [(apply str (range 50)) "bar" "baz"] "|||  (foo 012345678910111213141516171819202122232425262728293031323334353637383940414243444546474849 bar baz)"))

(def arg1 "dasdajlksjdalsjdlajasdasasdasdasdsdads")
(def arg2 "28347029384-asdaasdassdasd128-08-asd2834209")
(def expected-trace-result
  "
(orchard.trace-test.sample-ns/qux \"dasdajlksjdalsjdlajasdasasdasdasdsdads\" \"28347029384-asdaasdassdasd128-08-asd2834209\")
│ 
│ (orchard.trace-test.sample-ns/foo \"dasdajlksjdalsjdlajasdasasdasdasdsdads\" \"28347029384-asdaasdassdasd128-08-asd2834209\")
│ │ 
│ │ (orchard.trace-test.sample-ns/bar \"dasdajlksjdalsjdlajasdasasdasdasdsdads\")
│ │ │ 
│ │ │ (orchard.trace-test.sample-ns/baz 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 ...)
│ │ │ │ 
│ │ │ └─→ nil
│ │ │ 
│ │ └─→ \"dasdajlksjdalsjdlajasdasasdasdasdsdadshello\"
│ │ 
│ └─→ \"dasdajlksjdalsjdlajasdasasdasdasdsdadshello9024382dsa-80-821dsadssadsaadsa-48392074382\"
│ 
└─→ \"dasdajlksjdalsjdlajasdasasdasdasdsdadshello9024382dsa-80-821dsadssadsaadsa-48392074382\"
")
(def expected-trace-result-no-foo
  "
(orchard.trace-test.sample-ns/qux \"dasdajlksjdalsjdlajasdasasdasdasdsdads\" \"28347029384-asdaasdassdasd128-08-asd2834209\")
│ 
│ (orchard.trace-test.sample-ns/bar \"dasdajlksjdalsjdlajasdasasdasdasdsdads\")
│ │ 
│ │ (orchard.trace-test.sample-ns/baz 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 ...)
│ │ │ 
│ │ └─→ nil
│ │ 
│ └─→ \"dasdajlksjdalsjdlajasdasasdasdasdsdadshello\"
│ 
└─→ \"dasdajlksjdalsjdlajasdasasdasdasdsdadshello9024382dsa-80-821dsadssadsaadsa-48392074382\"
")
(def vars [#'sample-ns/baz #'sample-ns/bar #'sample-ns/foo #'sample-ns/qux])

(deftest basic-test
  (run! sut/trace-var* vars)
  (is (= expected-trace-result (with-out-str-rn (sample-ns/qux arg1 arg2))))

  (run! sut/untrace-var* vars)
  (is (= "" (with-out-str-rn (sample-ns/qux arg1 arg2))))

  (run! sut/trace-var* vars)
  (sut/untrace-var* #'sample-ns/foo)
  (is (= expected-trace-result-no-foo (with-out-str-rn (sample-ns/qux arg1 arg2))))

  (sut/trace-ns* 'orchard.trace-test.sample-ns)
  (is (= expected-trace-result (with-out-str-rn (sample-ns/qux arg1 arg2))))

  (sut/untrace-ns* 'orchard.trace-test.sample-ns)
  (is (= "" (with-out-str-rn (sample-ns/qux arg1 arg2))))

  (sut/trace-ns* 'orchard.trace-test.sample-ns)
  (sut/untrace-all)
  (is (= "" (with-out-str-rn (sample-ns/qux arg1 arg2)))))

(deftest fibo-test
  (sut/trace-ns* 'orchard.trace-test.sample-ns)

  (is (= "
(orchard.trace-test.sample-ns/fibo 5)
│ 
│ (orchard.trace-test.sample-ns/fibo 3)
│ │ 
│ │ (orchard.trace-test.sample-ns/fibo 1)
│ │ │ 
│ │ └─→ 1
│ │ 
│ │ (orchard.trace-test.sample-ns/fibo 2)
│ │ │ 
│ │ │ (orchard.trace-test.sample-ns/fibo 0)
│ │ │ │ 
│ │ │ └─→ 1
│ │ │ 
│ │ │ (orchard.trace-test.sample-ns/fibo 1)
│ │ │ │ 
│ │ │ └─→ 1
│ │ │ 
│ │ └─→ 2
│ │ 
│ └─→ 3
│ 
│ (orchard.trace-test.sample-ns/fibo 4)
│ │ 
│ │ (orchard.trace-test.sample-ns/fibo 2)
│ │ │ 
│ │ │ (orchard.trace-test.sample-ns/fibo 0)
│ │ │ │ 
│ │ │ └─→ 1
│ │ │ 
│ │ │ (orchard.trace-test.sample-ns/fibo 1)
│ │ │ │ 
│ │ │ └─→ 1
│ │ │ 
│ │ └─→ 2
│ │ 
│ │ (orchard.trace-test.sample-ns/fibo 3)
│ │ │ 
│ │ │ (orchard.trace-test.sample-ns/fibo 1)
│ │ │ │ 
│ │ │ └─→ 1
│ │ │ 
│ │ │ (orchard.trace-test.sample-ns/fibo 2)
│ │ │ │ 
│ │ │ │ (orchard.trace-test.sample-ns/fibo 0)
│ │ │ │ │ 
│ │ │ │ └─→ 1
│ │ │ │ 
│ │ │ │ (orchard.trace-test.sample-ns/fibo 1)
│ │ │ │ │ 
│ │ │ │ └─→ 1
│ │ │ │ 
│ │ │ └─→ 2
│ │ │ 
│ │ └─→ 3
│ │ 
│ └─→ 5
│ 
└─→ 8
"
         (with-out-str-rn (sample-ns/fibo 5))))

  (is (= "
(orchard.trace-test.sample-ns/fibo2 0 1 10)
│ 
│ (orchard.trace-test.sample-ns/fibo2 1 1 9)
│ │ 
│ │ (orchard.trace-test.sample-ns/fibo2 1 2 8)
│ │ │ 
│ │ │ (orchard.trace-test.sample-ns/fibo2 2 3 7)
│ │ │ │ 
│ │ │ │ (orchard.trace-test.sample-ns/fibo2 3 5 6)
│ │ │ │ │ 
│ │ │ │ │ (orchard.trace-test.sample-ns/fibo2 5 8 5)
│ │ │ │ │ │ 
│ │ │ │ │ │ (orchard.trace-test.sample-ns/fibo2 8 13 4)
│ │ │ │ │ │ │ 
│ │ │ │ │ │ │ (orchard.trace-test.sample-ns/fibo2 13 21 3)
│ │ │ │ │ │ │ │ 
│ │ │ │ │ │ │ │ (orchard.trace-test.sample-ns/fibo2 21 34 2)
│ │ │ │ │ │ │ │ │ 
│ │ │ │ │ │ │ │ │ (orchard.trace-test.sample-ns/fibo2 34 55 1)
│ │ │ │ │ │ │ │ │ │ 
│ │ │ │ │ │ │ │ │ │ (orchard.trace-test.sample-ns/fibo2 55 89 0)
│ │ │ │ │ │ │ │ │ │ │ 
│ │ │ │ │ │ │ │ │ │ └─→ 89
│ │ │ │ │ │ │ │ │ │ 
│ │ │ │ │ │ │ │ │ └─→ 89
│ │ │ │ │ │ │ │ │ 
│ │ │ │ │ │ │ │ └─→ 89
│ │ │ │ │ │ │ │ 
│ │ │ │ │ │ │ └─→ 89
│ │ │ │ │ │ │ 
│ │ │ │ │ │ └─→ 89
│ │ │ │ │ │ 
│ │ │ │ │ └─→ 89
│ │ │ │ │ 
│ │ │ │ └─→ 89
│ │ │ │ 
│ │ │ └─→ 89
│ │ │ 
│ │ └─→ 89
│ │ 
│ └─→ 89
│ 
└─→ 89
"
         (with-out-str-rn (sample-ns/fibo2 0 1 10)))))

(defn function-that-prints []
  (let [example-data (into {} (map #(vector % :val) (range 1000)))
        example-edn (pr-str example-data)]
    (clojure.edn/read-string example-edn)))

(deftest dont-restrict-printing-within-wrapped-function-test
  (is (map? (function-that-prints)))
  (sut/trace-var* 'orchard.trace-test/function-that-prints)
  (is (map? (function-that-prints))
      "Map is read back correctly, so it was printed in full."))

(deftest trace-listener-test
  ;; Earlier tests may leave the sample namespace traced; start from a clean
  ;; slate so only `bar` is wrapped and exactly its two events show up.
  (sut/untrace-all)
  (testing ":listeners mode emits structured events instead of printing"
    (let [events   (atom [])
          listener (fn [e] (swap! events conj e))]
      (sut/set-output-mode! :listeners)
      (sut/add-trace-listener listener)
      (sut/trace-var* #'sample-ns/bar)
      (try
        (let [out (with-out-str-rn (sample-ns/bar arg1))]
          (is (= "" out) "nothing is printed in :listeners mode")
          (is (= 2 (count @events)))
          (let [[call ret] @events]
            (is (= :call (:phase call)))
            (is (= :return (:phase ret)))
            (is (= "orchard.trace-test.sample-ns/bar" (:name call) (:name ret)))
            (is (= (:id call) (:id ret)) "call and return share an id")
            (is (= 0 (:depth call)))
            (is (= 1 (count (:args call))))
            (is (every? string? (:args call)))
            (is (string? (:value ret)))
            (is (re-find #"hello" (:value ret)))))
        (finally
          (sut/remove-trace-listener listener)
          (sut/untrace-var* #'sample-ns/bar)
          (sut/set-output-mode! :repl)))))

  (testing "the default :repl mode emits no events"
    (let [events   (atom [])
          listener (fn [e] (swap! events conj e))]
      (sut/add-trace-listener listener)
      (sut/trace-var* #'sample-ns/bar)
      (try
        (sample-ns/bar arg1)
        (is (empty? @events))
        (finally
          (sut/remove-trace-listener listener)
          (sut/untrace-var* #'sample-ns/bar))))))

(defn function-that-throws [x]
  (throw (ex-info "Boom" {:x x})))

(deftest throw-test
  (sut/trace-var* #'function-that-throws)
  (try
    (testing "printed, with the exception going on"
      (is (= "
(orchard.trace-test/function-that-throws 1)
│ 
└─✗ clojure.lang.ExceptionInfo: Boom
"
             (with-out-str-rn
               (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Boom"
                                     (function-that-throws 1)))))))
    (testing "as an event, which shares the call's id"
      (let [events (atom [])
            listener #(swap! events conj %)]
        (sut/set-output-mode! :listeners)
        (sut/add-trace-listener listener)
        (try
          (is (thrown? clojure.lang.ExceptionInfo (function-that-throws 1)))
          (let [[call thrown] @events]
            (is (= :throw (:phase thrown)))
            (is (= (:id call) (:id thrown)))
            (is (= "clojure.lang.ExceptionInfo: Boom" (:error thrown))))
          (finally
            (sut/remove-trace-listener listener)
            (sut/set-output-mode! :repl)))))
    (finally
      (sut/untrace-var* #'function-that-throws))))

(deftest trace-what-tracing-calls-test
  ;; Calling any of these through its var used to recurse into tracing.
  (doseq [mode [:repl :listeners]
          [v f expected] [[#'clojure.core/str #(str "a" 1) "a1"]
                          [#'clojure.core/apply #(apply + [1 2]) 3]
                          [#'clojure.core/map #(doall (map inc [1 2])) [2 3]]
                          [#'clojure.core/hash-map #(hash-map :a 1) {:a 1}]
                          [#'clojure.core/swap! #(swap! (atom 1) inc) 2]
                          [#'clojure.core/into #(into [] [1]) [1]]]]
    (testing (str v " in " mode)
      (let [events (atom [])
            listener #(swap! events conj %)
            result (atom nil)]
        (sut/set-output-mode! mode)
        (sut/add-trace-listener listener)
        (try
          (let [out (with-out-str-rn
                      (sut/trace-var* v)
                      (try (reset! result (f))
                           (catch StackOverflowError e (reset! result e))
                           (finally (sut/untrace-var* v))))]
            (is (= expected @result))
            (is (str/includes? (if (= :repl mode) out (pr-str @events))
                               (str (symbol v)))))
          (finally
            (sut/remove-trace-listener listener)
            (sut/set-output-mode! :repl)))))))

(deftest lazy-value-test
  ;; Printing a lazy value realizes it, and the traced fns that calls outside
  ;; the tooling namespaces get traced as usual.
  (let [ns (create-ns 'trace-test.lazy)
        g (intern ns 'g inc)
        f (intern ns 'f (fn [n] (map g (range n))))]
    (sut/trace-var* g)
    (sut/trace-var* f)
    (try
      (is (= 200 (count (re-seq #"\(trace-test.lazy/g \d+\)"
                                (with-out-str-rn (doall (f 200)))))))
      (finally
        (sut/untrace-var* g)
        (sut/untrace-var* f)
        (remove-ns 'trace-test.lazy)))))
