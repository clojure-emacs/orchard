(ns orchard.meta-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer :all]
   [orchard.clojuredocs :as docs]
   [orchard.meta :as m]))

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
  (with-redefs [docs/test-remote-url (constantly [true])
                docs/cache-file-name "target/clojuredocs/export.edn"
                docs/default-edn-file-url (io/resource "clojuredocs/export.edn")]
    (testing "Names are correct for `&`, `catch`, `finally`"
      (is (= '& (:name (m/special-sym-meta '&))))
      (is (= 'catch (:name (m/special-sym-meta 'catch))))
      (is (= 'finally (:name (m/special-sym-meta 'finally)))))

    (testing ":see-also metadata is attached"
      (is (not-empty (:see-also (m/special-sym-meta 'if)))))

    (testing "Name is correct for `clojure.core/import*`"
    ;; Only compiler special to be namespaced
      (is (= 'clojure.core/import* (:name (m/special-sym-meta 'clojure.core/import*)))))

    (testing "No ns for &, which uses fn's info"
      (is (nil? (:ns (m/special-sym-meta '&)))))

    (testing "Returns nil for unknown symbol"
      (is (nil? (m/special-sym-meta 'unknown))))))

(deftest special-sym-meta-without-see-also-test
  (with-redefs [docs/test-remote-url (constantly [false (Exception. "dummy")])
                docs/cache-file-name "target/clojuredocs/export.edn"]
    (docs/clean-cache!)
    (testing "Attaching see-also is skipped"
      (is (empty? (:see-also (m/special-sym-meta 'if)))))))

(deftest var-meta-test
  (with-redefs [docs/test-remote-url (constantly [true])
                docs/cache-file-name "target/clojuredocs/export.edn"
                docs/default-edn-file-url (io/resource "clojuredocs/export.edn")]
    ;; Test files can't be found on the class path.
    (is (:file (m/var-meta #'m/var-meta)))
    (testing "Includes spec information"
      (is (or (contains? (m/var-meta (resolve 'let)) :spec)
              (nil? (resolve 'clojure.spec.alpha/spec)))))
    (testing "Includes see-also information from clojure docs"
      (is (contains? (m/var-meta (resolve 'clojure.set/union)) :see-also)))
    (is (re-find #"string\.clj"
                 (:file (#'m/maybe-add-file
                         {:ns (find-ns 'clojure.string)}))))
    (is (not (re-find #"/form-init[^/]*$"
                      (:file (m/var-meta
                              (eval '(do (in-ns 'clojure.string)
                                         (def pok 10))))))))))

(deftest var-meta-without-see-also-test
  (with-redefs [docs/test-remote-url (constantly [false (Exception. "dummy")])
                docs/cache-file-name "target/clojuredocs/export.edn"]
    (docs/clean-cache!)
    (testing "Including see-also is skipped"
      (is (not (contains? (m/var-meta (resolve 'clojure.set/union)) :see-also))))))
