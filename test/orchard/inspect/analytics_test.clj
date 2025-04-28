(ns orchard.inspect.analytics-test
  (:require
   [clojure.test :refer [deftest testing]]
   [matcher-combinators.matchers :as mc]
   [orchard.inspect.analytics :refer [analytics]]
   [orchard.test.util :refer [is+]]))

(defn- approx [val] (mc/within-delta 0.01 val))

(deftest numbers-test
  (is+ {:count 1000
        :types {java.lang.Long 1000}
        :frequencies map?
        :numbers {:n 1000, :zeros 1, :max 999, :min 0, :mean 499.5}}
       (analytics (range 1000)))

  (is+ {:count 100
        :types {java.lang.Integer 50, java.lang.Long 50}
        :frequencies {0 50, 1 50}
        :numbers {:n 100, :zeros 50, :max 1, :min 0, :mean 0.5}}
       (analytics (concat (map int (repeat 50 0)) (repeat 50 1))))

  (is+ {:count 6
        :types {java.lang.Long 3, nil 3}
        :frequencies {nil 3, 0 1, 1 1, 2 1}
        :numbers {:n 3, :zeros 1, :max 2, :min 0, :mean 1.0}}
       (analytics [0 nil 1 nil 2 nil]))

  (is+ {:count 5
        :types {java.lang.Long 5}
        :frequencies {0 1, 1 1, 2 1, 3 1, 4 1}
        :numbers {:n 5, :zeros 1, :max 4, :min 0, :mean 2.0}}
       (analytics (range 5))))

(deftest strings-test
  (is+ {:count 100
        :types {java.lang.String 100}
        :frequencies map?
        :strings {:n 100, :blank 0, :ascii 100, :max-len 2, :min-len 1, :avg-len (approx 1.9)}}
       (analytics (map str (range 100))))

  (is+ {:count 6,
        :types {java.lang.String 5, nil 1}
        :frequencies map?
        :strings {:n 5, :blank 3, :ascii 4, :max-len 6, :min-len 0, :avg-len (approx 2.8)}}
       (analytics [nil "" " " "  " "hello" "привіт"])))

(deftest colls-stats
  (is+ {:count 20
        :types {clojure.lang.LongRange 19, clojure.lang.PersistentList$EmptyList 1}
        :frequencies map?
        :collections {:n 20, :empty 1, :max-size 19, :min-size 0, :avg-size 9.5}}
       (analytics (map range (range 20)))))

(deftest heterogeneous-list-stats
  (is+ {:count 90,
        :types {java.lang.Long 20, java.lang.Integer 20, java.lang.String 20, clojure.lang.PersistentVector 20, nil 10}
        :frequencies {nil 10, [42] 10, [] 10 ;; and others
                      }
        :numbers {:n 40, :zeros 2, :max 19, :min 0, :mean 9.5}
        :strings {:n 20, :blank 0, :ascii 20, :max-len 2, :min-len 1, :avg-len 1.5}
        :collections {:n 20, :empty 10, :max-size 1, :min-size 0, :avg-size 0.5}}
       (analytics (shuffle (concat (range 20)
                                   (map int (range 20))
                                   (map str (range 20))
                                   (repeat 10 [])
                                   (repeat 10 [42])
                                   (repeat 10 nil))))))

(deftest keyvals-test
  (is+ {:count 100
        :keys {:types {java.lang.Long 100}
               :frequencies map?
               :numbers {:n 100, :zeros 1, :max 99, :min 0, :mean 49.5}}
        :values {:types {java.lang.String 100}
                 :frequencies map?
                 :strings {:n 100, :blank 0, :ascii 100, :max-len 2, :min-len 1, :avg-len (approx 1.9)}}}
       (analytics (zipmap (range 100) (map str (range 100))))))

(deftest list-of-tuples-test
  (is+ {:count 100
        :tuples [{:types {java.lang.Long 100}
                  :numbers {:n 100, :zeros 1, :max 99, :min 0, :mean 49.5}}
                 {:types {java.lang.String 100}
                  :strings {:n 100, :blank 0, :ascii 100, :max-len 2, :min-len 1, :avg-len (approx 1.9)}}
                 {:types {nil 50, java.lang.Long 50}
                  :frequencies {nil 50, 42 50}
                  :numbers {:n 50, :zeros 0, :max 42, :min 42, :mean 42.0}}]}
       (analytics (map #(cond-> [% (str %)]
                          (odd? %) (conj 42))
                       (range 100)))))

(deftest list-of-records-test
  (testing "analytics on a list of records analyzes values for each unique key individually"
    (is+ {:count 100
          :by-key {:s {:types {java.lang.String 100}
                       :frequencies map?
                       :strings {:n 100, :blank 0, :ascii 50, :max-len 7, :min-len 5, :avg-len (approx 6.4)}}
                   :kw {:types {nil 66, clojure.lang.Keyword 34}, :frequencies {nil 66, :occasional 34}}
                   :i {:types {java.lang.Long 100}
                       :numbers {:n 100, :zeros 1, :max 99, :min 0, :mean 49.5}}}}
         (analytics (map #(cond-> {:i %
                                   :s (str "test" % (when (even? %) "ї"))}
                            (zero? (mod % 3)) (assoc :kw :occasional))
                         (range 100))))))

(deftest cutoff-test
  (testing "when the collection size is within cutoff, :cutoff? field is not returned"
    (is+ {:cutoff? mc/absent} (analytics (range 100))))

  (testing "when above cutoff, :cutoff? true is returned, and only #cutoff items are analyzed"
    (is+ {:cutoff? true
          :count 100
          :numbers {:max 99}}
         (binding [orchard.inspect.analytics/*size-cutoff* 100]
           (analytics (range 200))))
    (is+ {:cutoff? true
          :count 100}
         (binding [orchard.inspect.analytics/*size-cutoff* 100]
           (analytics (repeat 101 "test"))))
    (is+ {:cutoff? true
          :count 100}
         (binding [orchard.inspect.analytics/*size-cutoff* 100]
           (analytics (repeat 100 {:foo 1 :bar 2}))))
    (is+ {:cutoff? true
          :count 100}
         (binding [orchard.inspect.analytics/*size-cutoff* 100]
           (analytics (repeat 100 [1 "test"]))))
    (is+ {:cutoff? true
          :count 100
          :keys {:types {java.lang.Long 100}}
          :values {:types {java.lang.Long 100}}}
         (binding [orchard.inspect.analytics/*size-cutoff* 100]
           (analytics (zipmap (range 200) (range 200)))))))
