(ns orchard.trace-test.sample-ns
  (:require clojure.string))

(defn baz [& _]
  nil)

(defn bar [a]
  (apply baz (range))
  (str a "hello"))

(defn foo [a b]
  (str (bar a) (clojure.string/reverse b)))

(defn qux [a b]
  (foo a b))

(defn fibo [n]
  (if (<= n 1)
    1
    (+ (fibo (- n 2)) (fibo (- n 1)))))

(defn fibo2 [a b n]
  (if (= n 0)
    b
    (fibo2 b (+ a b) (dec n))))
