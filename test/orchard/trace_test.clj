(ns orchard.trace-test
  (:require
   [clojure.test :as t :refer [is are deftest]]
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
  (is (= expected-trace-result (with-out-str (sample-ns/qux arg1 arg2))))

  (run! sut/untrace-var* vars)
  (is (= "" (with-out-str (sample-ns/qux arg1 arg2))))

  (run! sut/trace-var* vars)
  (sut/untrace-var* #'sample-ns/foo)
  (is (= expected-trace-result-no-foo (with-out-str (sample-ns/qux arg1 arg2))))

  (sut/trace-ns* 'orchard.trace-test.sample-ns)
  (is (= expected-trace-result (with-out-str (sample-ns/qux arg1 arg2))))

  (sut/untrace-ns* 'orchard.trace-test.sample-ns)
  (is (= "" (with-out-str (sample-ns/qux arg1 arg2))))

  (sut/trace-ns* 'orchard.trace-test.sample-ns)
  (sut/untrace-all)
  (is (= "" (with-out-str (sample-ns/qux arg1 arg2)))))

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
         (with-out-str (sample-ns/fibo 5))))

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
         (with-out-str (sample-ns/fibo 5))))

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
         (with-out-str (sample-ns/fibo2 0 1 10)))))
