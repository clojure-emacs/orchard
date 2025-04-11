(ns orchard.pp.pp-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [orchard.pp.test :refer [pp]]))

(defn ^:private q
  []
  clojure.lang.PersistentQueue/EMPTY)

(deftest pprint
  (is (= "{}\n" (pp {})))
  (is (= "[nil nil]\n" (pp [nil nil])))
  (is (= "{:a 1}\n" (pp {:a 1})))
  (is (= "(1 nil)\n" (pp '(1 nil))))
  (is (= "{:a 1, :b 2, :c 3, :d 4}\n" (pp {:a 1 :b 2 :c 3 :d 4} :max-width 24)))

  (is (= "{:args\n [{:op :var,\n   :assignable? true}]}\n"
         (pp {:args [{:op :var :assignable? true}]} :max-width 24)))

  (is (= "{:a 1,\n :b 2,\n :c 3,\n :d 4,\n :e 5}\n"
         (pp {:a 1 :b 2 :c 3 :d 4 :e 5} :max-width 24)))

  (is (= "{:a\n 1,\n :b\n 2,\n :c\n 3,\n :d\n 4}\n"
         (pp {:a 1 :b 2 :c 3 :d 4} :max-width 0)))

  (is (= "{:a 1,\n :b 2,\n :c 3,\n :d 4,\n :e {:f 6}}\n"
         (pp {:a 1 :b 2 :c 3 :d 4 :e {:f 6}} :max-width 24)))

  (is (= "{:a 1,\n :b 2,\n :c 3,\n :d 4,\n :e\n {:a 1,\n  :b 2,\n  :c 3,\n  :d 4,\n  :e\n  {:f 6,\n   :g 7,\n   :h 8,\n   :i 9,\n   :j 10}}}\n"
         (pp {:a 1
              :b 2
              :c 3
              :d 4
              :e {:a 1 :b 2 :c 3 :d 4 :e {:f 6 :g 7 :h 8 :i 9 :j 10}}}
             :max-width 24)))

  ;; Queues
  (is (= "<-()-<\n" (pp (q))))
  (is (= "<-(1)-<\n" (pp (conj (q) 1))))
  (is (= "<-(1\n   2\n   3)-<\n" (pp (conj (q) 1 2 3) :max-width 1)))
  (is (= "<-(1 ...)-<\n" (pp (conj (q) 1 2 3) :print-length 1)))
  (is (= "<-(1 2 3)-<\n" (pp (conj (q) 1 2 3) :print-level 1)))
  (is (= "<-(1 ...)-<\n" (pp (conj (q) 1 2 3) :print-length 1 :print-level 1)))
  (is (= "<-(1\n   2\n   3)-<\n" (pp (conj (q) 1 2 3) :max-width 6)))

  ;; Max width
  (is (= "{:a\n 1,\n :b\n 2,\n :c\n 3,\n :d\n 4}\n"
         (pp {:a 1 :b 2 :c 3 :d 4} :max-width 0)))

  ;; Meta
  (is (= "^{:b 2} {:a 1}\n"
         (pp (with-meta {:a 1} {:b 2}) :print-meta true)))
  (is (= "^{:b\n  2}\n{:a\n 1}\n"
         (pp (with-meta {:a 1} {:b 2}) :print-meta true :max-width 2)))

  ;; Print level
  (is (= "#\n" (pp {} :print-level 0)))
  (is (= "#\n" (pp {:a 1} :print-level 0)))
  (is (= "{#}\n" (pp {:a {:b 2}} :print-level 1)))
  (is (= "{:a #}\n" (pp {:a {:b 2}} :print-level 2)))
  (is (= "{:a {#}}\n" (pp {:a {:b 2}} :print-level 3)))
  (is (= "{#}\n" (pp {{:a 1} :b} :print-level 1)))
  (is (= "{# :b}\n" (pp {{:a 1} :b} :print-level 2)))
  (is (= "{{#} :b}\n" (pp {{:a 1} :b} :print-level 3)))
  (is (= "#\n" (pp '(:a (:b (:c (:d)))) :print-level 0)))
  (is (= "(:a #)\n" (pp '(:a (:b (:c (:d)))) :print-level 1)))
  (is (= "(:a (:b #))\n" (pp '(:a (:b (:c (:d)))) :print-level 2)))
  (is (= "(:a (:b (:c #)))\n" (pp '(:a (:b (:c (:d)))) :print-level 3)))
  (is (= "(:a (:b (:c (:d))))\n" (pp '(:a (:b (:c (:d)))) :print-level 4)))
  (is (= "(:a (:b (:c (:d))))\n" (pp '(:a (:b (:c (:d)))) :print-level 5)))

  ;; Print length
  (is (= "(...)\n" (pp '() :print-length 0)))
  (is (= "[...]\n" (pp [] :print-length 0)))
  (is (= "#{...}\n" (pp #{} :print-length 0)))
  (is (= "{...}\n" (pp {} :print-length 0)))
  (is (= "(...)\n" (pp (cons 1 '()) :print-length 0))) ; Cons
  (is (= "(...)\n" (pp (range) :print-length 0)))
  (is (= "(0 ...)\n" (pp (range) :print-length 1)))
  (is (= "(...)\n" (pp '(1 2 3) :print-length 0)))
  (is (= "(1 ...)\n" (pp '(1 2 3) :print-length 1)))
  (is (= "(1 2 ...)\n" (pp '(1 2 3) :print-length 2)))
  (is (= "(1 2 3)\n" (pp '(1 2 3) :print-length 3)))
  (is (= "(1 2 3)\n" (pp '(1 2 3) :print-length 4)))

  ;; Print level and print length
  (is (= "#\n" (pp {} :print-level 0 :print-length 0)))
  (is (= "{...}\n" (pp {} :print-level 1 :print-length 0)))
  (is (= "#\n" (pp {} :print-level 0 :print-length 1)))
  (is (= "{}\n" (pp {} :print-level 1 :print-length 1)))

  (is (= "#\n" (pp {:a 1 :b 2} :print-level 0 :print-length 0)))
  (is (= "{...}\n" (pp {:a 1 :b 2} :print-level 1 :print-length 0)))
  (is (= "#\n" (pp {:a 1 :b 2} :print-level 0 :print-length 1)))
  (is (= "{#, ...}\n" (pp {:a 1 :b 2} :print-level 1 :print-length 1)))

  ;; Width
  (is (= "{[]\n [ab000000000000000000000000000000000000000000000000000000000000000N]}\n"
         (pp {[]
              ['ab000000000000000000000000000000000000000000000000000000000000000N]}
             :max-width 72)))

  ;; Reader macros
  (is (= "#'clojure.core/map\n" (pp #'map)))
  (is (= "(#'map)\n" (pp '(#'map))))
  (is (= "#{#'mapcat #'map}\n" (pp '#{#'map #'mapcat})))

  (is (= "{:arglists '([xform* coll]), :added \"1.7\"}\n"
         (pp '{:arglists (quote ([xform* coll])) :added "1.7"})))

  (is (= "@(foo)\n" (pp '@(foo))))
  (is (= "'foo\n" (pp ''foo)))
  (is (= "~foo\n" (pp '~foo)))

  (is (= "('#{boolean\n    char\n    floats})\n"
         (pp '('#{boolean char floats}) :max-width 23)))

  (is (= "#\n"
         (pp '('#{boolean char floats}) :max-width 23 :print-level 0)))

  (is (= "(...)\n"
         (pp '('#{boolean char floats}) :max-width 23 :print-length 0)))

  (is (= "('#{boolean\n    char\n    floats})\n"
         (pp '('#{boolean char floats}) :max-width 23 :print-length 3)))

  ;; Namespace maps
  (is (= "#:a{:b 1}\n" (pp {:a/b 1} :print-namespace-maps true)))
  (is (= "#:a{:b 1, :c 2}\n" (pp {:a/b 1 :a/c 2} :print-namespace-maps true)))
  (is (= "{:a/b 1, :c/d 2}\n" (pp {:a/b 1 :c/d 2} :print-namespace-maps true)))
  (is (= "#:a{:b #:a{:b 1}}\n" (pp {:a/b {:a/b 1}} :print-namespace-maps true)))
  (is (= "#:a{b 1}\n" (pp {'a/b 1} :print-namespace-maps true)))
  (is (= "#:a{b 1, c 3}\n" (pp {'a/b 1 'a/c 3} :print-namespace-maps true)))
  (is (= "{a/b 1, c/d 2}\n" (pp {'a/b 1 'c/d 2} :print-namespace-maps true)))
  (is (= "#:a{b #:a{b 1}}\n" (pp {'a/b {'a/b 1}} :print-namespace-maps true)))
  (is (= "{:a/b 1}\n" (pp {:a/b 1} :print-namespace-maps false)))
  (is (= "{:a/b 1, :a/c 2}\n" (pp {:a/b 1 :a/c 2} :print-namespace-maps false)))
  (is (= "{:a/b 1, :c/d 2}\n" (pp {:a/b 1 :c/d 2} :print-namespace-maps false)))
  (is (= "{:a/b {:a/b 1}}\n" (pp {:a/b {:a/b 1}} :print-namespace-maps false)))
  (is (= "{a/b 1}\n" (pp {'a/b 1} :print-namespace-maps false)))
  (is (= "{a/b 1, a/c 3}\n" (pp {'a/b 1 'a/c 3} :print-namespace-maps false)))
  (is (= "{a/b 1, c/d 2}\n" (pp {'a/b 1 'c/d 2} :print-namespace-maps false)))
  (is (= "{a/b {a/b 1}}\n" (pp {'a/b {'a/b 1}} :print-namespace-maps false)))
  (is (= "#:a{:b 1,\n    :c 2}\n" (pp #:a{:b 1 :c 2} :max-width 14 :print-namespace-maps true)))

  ;; Custom tagged literals
  ;; (is (= "#time/date \"2023-10-02\"\n" (pp #time/date "2023-10-02")))

  ;; Sorted maps
  (is (= "{}\n" (pp (sorted-map))))
  (is (= "{:a 1, :b 2}\n" (pp (sorted-map :a 1 :b 2))))
  (is (= "{:a 1, ...}\n" (pp (sorted-map :a 1 :b 2) :print-length 1)))
  (is (= "{:a 1,\n :b 2}\n" (pp (sorted-map :a 1 :b 2) :max-width 7)))

  ;; Sorted sets
  (is (= "#{}\n" (pp (sorted-set))))
  (is (= "#{1 2 3}\n" (pp (sorted-set 1 2 3))))
  (is (= "#{1 ...}\n" (pp (sorted-set 1 2 3) :print-length 1)))
  (is (= "#{1\n  2\n  3}\n" (pp (sorted-set 1 2 3) :max-width 3)))

  ;; Symbolic
  (is (= "##Inf\n" (pp ##Inf)))
  (is (= "##-Inf\n" (pp ##-Inf)))
  (is (= "##NaN\n" (pp ##NaN)))

  ;; Map entries
  (is (= "[:a 1]\n" (pp (find {:a 1} :a))))
  (is (= "[[:a 1]]\n" (pp [(find {:a 1} :a)])))
  (is (= "([:a 1])\n" (pp (list (find {:a 1} :a)))))
  (is (= "#{[:a 1]}\n" (pp #{(find {:a 1} :a)})))
  (is (= "#\n" (pp (find {:a 1} :a) :print-level 0)))
  (is (= "[:a 1]\n" (pp (find {:a 1} :a) :print-level 1)))
  (is (= "[...]\n" (pp (find {:a 1} :a) :print-length 0)))
  (is (= "[:a ...]\n" (pp (find {:a 1} :a) :print-length 1)))
  (is (= "[...]\n" (pp (find {:a 1} :a) :print-level 1 :print-length 0)))
  (is (= "#\n" (pp (find {[:a 1] [:b 1]} [:a 1]) :print-level 0)))
  (is (= "#\n" (pp (find {:a 1} :a) :print-level 0 :print-length 1)))
  (is (= "#\n" (pp (find {[:a 1] [:b 1]} [:a 1]) :print-level 0 :print-length 0)))
  (is (= "[# #]\n" (pp (find {[:a 1] [:b 1]} [:a 1]) :print-level 1)))
  (is (= "[# ...]\n" (pp (find {[:a 1] [:b 1]} [:a 1]) :print-level 1 :print-length 1)))
  (is (= "[...]\n" (pp (find {[:a 1] [:b 1]} [:a 1]) :print-length 0 :print-level 1)))
  (is (= "[...]\n" (pp (find {[:a 1] [:b 1]} [:a 1]) :print-length 0)))
  (is (= "[[:a ...] ...]\n" (pp (find {[:a 1] [:b 1]} [:a 1]) :print-length 1)))
  (is (= "[[:a 1] [:b 1]]\n" (pp (find {[:a 1] [:b 1]} [:a 1]) :print-level 2)))
  (is (= "[[:a 1] [:b 1]]\n" (pp (find {[:a 1] [:b 1]} [:a 1]) :print-level 3)))
  (is (= "[0\n 1]\n" (pp (find {0 1} 0) :max-width 2))))

(deftest pprint-meta
  ;; clojure.pprint prints this incorrectly with meta
  (is (= "{:a 1}\n"
         (pp (with-meta {:a 1} {:b 2}) :print-meta true :print-readably false)))

  (is (= "{:a 1}\n"
         (pp (with-meta {:a 1} {}) :print-meta true))))

(deftest pprint-reader-macro-edge-cases
  ;; do not print the reader macro character if the collection following the
  ;; character exceeds print level
  (is (= "#\n" (pp '('#{boolean char floats}) :print-level 0)))
  (is (= "(#)\n" (pp '('#{boolean char floats}) :print-level 1)))
  (is (= "(#)\n" (pp '('#{boolean char floats}) :print-level 1 :print-length 1)))

  ;; reader macro characters do not count towards *print-length*
  (is (= "(...)\n" (pp '('#{boolean char floats}) :print-length 0)))
  (is (= "('#{boolean ...})\n" (pp '('#{boolean char floats}) :print-length 1))))

(deftest map-entry-separator
  (is (= "{:a 1, :b 2}\n" (pp {:a 1 :b 2})))
  (is (= "{:a 1, :b 2}\n" (pp {:a 1 :b 2} :map-entry-separator ",")))
  (is (= "{:a 1,,, :b 2}\n" (pp {:a 1 :b 2} :map-entry-separator ",,,")))
  (is (= "{:a 1,,,\n :b 2}\n" (pp {:a 1 :b 2} :max-width 8 :map-entry-separator ",,,")))
  (is (= "{:a 1 :b 2}\n" (pp {:a 1 :b 2} :map-entry-separator "")))
  (is (= "{:a 1\n :b 2}\n" (pp {:a 1 :b 2} :max-width 7 :map-entry-separator ""))))

;; TODO: Test custom types as with Clojure

;; With infinite max width, prints everything the same way as prn.
(defspec print-linear 10000
  (for-all [x gen/any-printable-equatable
            print-namespace-maps gen/boolean]
           (= (pp x :print-namespace-maps print-namespace-maps :max-width ##Inf)
              (with-out-str
                (binding [*print-namespace-maps* print-namespace-maps]
                  (prn x))))))
