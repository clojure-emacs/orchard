(ns orchard.pp.pp-cpp-test
  (:require [clojure.test :refer [deftest]]
            [orchard.pp.test :refer [$]]))

;; This namespace is a holdover from the time when pp only supported Clojure.
;; It compares the output of clojure.pprint/pprint to the output of
;; orchard.pp.pp/pprint.
;;
;; It is largely superseded by orchard.pp.pp-test, and is probably OK to
;; remove.

(comment ($ {:a 1}))

(deftest pprint
  ;; Basic
  ($ {})
  ($ [nil nil])
  ($ {:a 1})
  ($ '(1 nil))
  ($ {:a 1 :b 2 :c 3 :d 4} :max-width 24)
  ($ {:args [{:op :var :assignable? true}]} :max-width 24)
  ($ {:a 1 :b 2 :c 3 :d 4 :e 5} :max-width 24)
  ($ {:a 1 :b 2 :c 3 :d 4} :max-width 0)
  ($ {:a 1 :b 2 :c 3 :d 4 :e {:f 6}} :max-width 24)
  ($ {:a 1
      :b 2
      :c 3
      :d 4
      :e {:a 1 :b 2 :c 3 :d 4 :e {:f 6 :g 7 :h 8 :i 9 :j 10}}}
     :max-width 24)

  ;; Queues
  ($ clojure.lang.PersistentQueue/EMPTY)
  ($ (conj clojure.lang.PersistentQueue/EMPTY 1))
  ($ (conj clojure.lang.PersistentQueue/EMPTY 1 2 3) :print-length 1)
  ($ (conj clojure.lang.PersistentQueue/EMPTY 1 2 3) :print-level 1)
  ($ (conj clojure.lang.PersistentQueue/EMPTY 1 2 3) :print-length 1 :print-level 1)
  ($ (conj clojure.lang.PersistentQueue/EMPTY 1 2 3) :max-width 6)

  ;; Max width
  ($ {:a 1 :b 2 :c 3 :d 4} :max-width 0)

  ;; Meta
  ($ (with-meta {:a 1} {:b 2}) :print-meta true)
  ($ (with-meta {:a 1} {:b 2}) :print-meta true :max-width 2)

  ;; Print level
  ($ {} :print-level 0)
  ($ {:a 1} :print-level 0)
  ($ {:a {:b 2}} :print-level 1)
  ($ {:a {:b 2}} :print-level 2)
  ($ {:a {:b 2}} :print-level 3)
  ($ {{:a 1} :b} :print-level 1)
  ($ {{:a 1} :b} :print-level 2)
  ($ {{:a 1} :b} :print-level 3)
  ($ '(:a (:b (:c (:d)))) :print-level 0)
  ($ '(:a (:b (:c (:d)))) :print-level 1)
  ($ '(:a (:b (:c (:d)))) :print-level 2)
  ($ '(:a (:b (:c (:d)))) :print-level 3)
  ($ '(:a (:b (:c (:d)))) :print-level 4)
  ($ '(:a (:b (:c (:d)))) :print-level 5)

  ;; Print length
  ($ '() :print-length 0)
  ($ [] :print-length 0)
  ($ #{} :print-length 0)
  ($ {} :print-length 0)
  ($ (range) :print-length 0)
  ($ (range) :print-length 1)
  ($ '(1 2 3) :print-length 0)
  ($ '(1 2 3) :print-length 1)
  ($ '(1 2 3) :print-length 2)
  ($ '(1 2 3) :print-length 3)
  ($ '(1 2 3) :print-length 4)

  ;; Print level and print length
  ($ {} :print-level 0 :print-length 0)
  ($ {} :print-level 1 :print-length 0)
  ($ {} :print-level 0 :print-length 1)
  ($ {} :print-level 1 :print-length 1)

  ($ {:a 1 :b 2} :print-level 0 :print-length 0)
  ($ {:a 1 :b 2} :print-level 1 :print-length 0)
  ($ {:a 1 :b 2} :print-level 0 :print-length 1)
  ($ {:a 1 :b 2} :print-level 1 :print-length 1)

  ;; Width
  ($ {[]
      [-1000000000000000000000000000000000000000000000000000000000000000N]}
     :max-width 72)

  ;; Reader macros
  ($ #'map)
  ($ '(#'map))
  ($ '#{#'map #'mapcat})
  ($ '{:arglists (quote ([xform* coll])) :added "1.7"})
  ($ '@(foo))
  ($ ''foo)
  ($ '~foo)
  ($ '('#{boolean char floats}) :max-width 23)
  ($ '('#{boolean char floats}) :max-width 23 :print-level 0)
  ($ '('#{boolean char floats}) :max-width 23 :print-length 0)
  ($ '('#{boolean char floats}) :max-width 23 :print-length 3)

  ;; Namespace maps
  ($ {:a/b 1} :print-namespace-maps true)
  ($ {:a/b 1 :a/c 2} :print-namespace-maps true)
  ($ {:a/b 1 :c/d 2} :print-namespace-maps true)
  ($ {:a/b {:a/b 1}} :print-namespace-maps true)
  ($ {'a/b 1} :print-namespace-maps true)
  ($ {'a/b 1 'a/c 3} :print-namespace-maps true)
  ($ {'a/b 1 'c/d 2} :print-namespace-maps true)
  ($ {'a/b {'a/b 1}} :print-namespace-maps true)
  ($ {:a/b 1} :print-namespace-maps false)
  ($ {:a/b 1 :a/c 2} :print-namespace-maps false)
  ($ {:a/b 1 :c/d 2} :print-namespace-maps false)
  ($ {:a/b {:a/b 1}} :print-namespace-maps false)
  ($ {'a/b 1} :print-namespace-maps false)
  ($ {'a/b 1 'a/c 3} :print-namespace-maps false)
  ($ {'a/b 1 'c/d 2} :print-namespace-maps false)
  ($ {'a/b {'a/b 1}} :print-namespace-maps false)
  ($ #:a{:b 1 :c 2} :max-width 14 :print-namespace-maps true)
  ($ #{'a/b 1 'a/c 2} :max-width 14 :print-namespace-maps true)

  ;; Custom tagged literals
  ;; ($ #time/date "2023-10-02")

  ;; Sorted maps
  ($ (sorted-map))
  ($ (sorted-map :a 1 :b 2))
  ($ (sorted-map :a 1 :b 2) :print-length 1)
  ($ (sorted-map :a 1 :b 2) :max-width 7)

  ;; Sorted sets
  ($ (sorted-set))
  ($ (sorted-set 1 2 3))
  ($ (sorted-set 1 2 3) :print-length 1)
  ($ (sorted-set 1 2 3) :max-width 3)

  ;; Symbolic
  ($ ##Inf)
  ($ ##-Inf)
  ($ ##NaN)

  ;; Map entries
  ($ (find {:a 1} :a))
  ($ (find {[:a 1] [:b 2]} [:a 1]))
  ($ (find {:a 1} :a) :print-level 0)
  ($ (find {:a 1} :a) :print-level 1)
  ($ (find {:a 1} :a) :print-length 0)
  ($ (find {:a 1} :a) :print-length 1)
  ($ (find {:a 1} :a) :print-level 0 :print-length 0)
  ($ (find {:a 1} :a) :print-level 0 :print-length 1)
  ($ (find {:a 1} :a) :print-level 1 :print-length 0)
  ($ (find {:a 1} :a) :print-level 1 :print-length 1)
  ($ (find {[:a 1] 1} [:a 1]) :print-level 1 :print-length 0)
  ($ (find {[:a 1] 1} [:a 1]) :print-level 1 :print-length 1)
  ($ (find {0 1} 0) :max-width 2)
  ($ [(find {:a 1} :a)])
  ($ #{(find {:a 1} :a)})
  ($ '((find {:a 1} :a))))

(deftest pprint-struct
  ($ (struct (create-struct :q/a :q/b :q/c) 1 2 3)))
