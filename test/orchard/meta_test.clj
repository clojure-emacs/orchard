(ns orchard.meta-test
  (:require
   [orchard.meta :as m]
   [clojure.java.classpath]
   [clojure.repl :as repl]
   [clojure.test :refer :all]))

(deftest merge-meta-test
  (testing "Always safe and preserves object"
    (are [form] (let [x form]
                  (= x (m/merge-meta x {:random 'meta})))
      0 1 1.0 (float 1) (double 1) 1M 1N 1/2
      'symbol :keyword
      (atom 10) (delay 10)
      [1 2 3] '(1 2 3)
      {1 2} #{1 2 3}))
  (testing "Applies meta"
    (are [form] (-> (m/merge-meta form {:random 'meta})
                    meta :random (= 'meta))
      ;; Keywords and numbers have no metadata.
      ;; Atoms and delays are mutable.
      'symbol
      [1 2 3] '(1 2 3)
      {1 2} #{1 2 3})))

(deftest strip-meta-test
  (testing "Always safe and preserves object"
    (are [form] (let [x form]
                  (= x (m/strip-meta (m/merge-meta x {:random 'meta}))))
      0 1 1.0 (float 1) (double 1) 1M 1N 1/2
      'symbol :keyword
      (atom 10) (delay 10)
      [1 2 3] '(1 2 3)
      {1 2} #{1 2 3}))
  (testing "Removes meta"
    (are [form] (-> (with-meta form {:random 'meta})
                    m/strip-meta meta :random not)
      ;; Keywords and numbers have no metadata.
      ;; Atoms and delays are mutable.
      'symbol
      [1 2 3] '(1 2 3)
      {1 2} #{1 2 3})))

(defn- test-fn "docstring"
  ([a b] nil)
  ([a] nil)
  ([]))

(defmacro test-macro [& x]
  `(do ~@x))

(deftest macroexpand-all-test
  (is (->> (m/macroexpand-all '(test-macro ^{:random meta} (hi)))
           second
           meta
           :random
           (= 'meta))))

(deftest special-sym-meta-test

  (testing "Names are correct for `&`, `catch`, `finally`"
    (is (= '& (:name (m/special-sym-meta '&))))
    (is (= 'catch (:name (m/special-sym-meta 'catch))))
    (is (= 'finally (:name (m/special-sym-meta 'finally)))))

  (testing "Name is correct for `clojure.core/import*`"
    ;; Only compiler special to be namespaced
    (is (= 'clojure.core/import* (:name (m/special-sym-meta 'clojure.core/import*)))))

  (testing "No ns for &, which uses fn's info"
    (is (nil? (:ns (m/special-sym-meta '&)))))

  (testing "Returns nil for unknown symbol"
    (is (nil? (m/special-sym-meta 'unknown)))))

(deftest var-meta-test
  ;; Test files can't be found on the class path.
  (is (:file (m/var-meta #'m/var-meta)))
  (is (re-find #"java\.classpath"
               (:file (#'m/maybe-add-file
                       {:ns (find-ns 'clojure.java.classpath)}))))
  (is (not (re-find #"/form-init[^/]*$"
                    (:file (m/var-meta
                            (eval '(do (in-ns 'clojure.java.classpath)
                                       (def pok 10)))))))))
