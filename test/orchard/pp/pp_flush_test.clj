(ns orchard.pp.pp-flush-test
  (:require [clojure.test :refer [deftest is testing]]
            [orchard.pp :as sut])
  (:import (java.io BufferedWriter StringWriter)))

(defn ^:private flush-observing-writer
  [writer]
  (let [a (atom 0)]
    [(proxy [BufferedWriter] [writer]
       (flush []
         (let [^BufferedWriter this this]
           (proxy-super flush)
           (swap! a inc))))
     a]))

(deftest flush-on-newline
  (testing "Does not flush after printing if *flush-on-newline* is nil"
    (let [sw (StringWriter.)
          [writer a] (flush-observing-writer sw)]
      (binding [*flush-on-newline* nil]
        (sut/pprint writer {:a 1 :b 2 :c 3 :d 4} {:max-width 10}))
      (is (= [0 ""] [@a (str sw)]))))

  (testing "Only flushes after printing the entire top-level form if *flush-on-newline* is true"
    ;; So when pretty-printing, *flush-on-newline* doesn't actually flush on
    ;; newline, only after printing the entire top-level form.
    (let [sw (StringWriter.)
          [writer a] (flush-observing-writer sw)]
      (binding [*flush-on-newline* true]
        (sut/pprint writer {:a 1 :b 2 :c 3 :d 4} {:max-width 10})
        (sut/pprint writer {:a 1 :b 2 :c 3 :d 4} {:max-width 10}))
      (is (= [2 "{:a 1,\n :b 2,\n :c 3,\n :d 4}\n{:a 1,\n :b 2,\n :c 3,\n :d 4}\n"]
             [@a (str sw)])))))
