(ns orchard.meta-test
  (:require
   [clojure.test :refer [are deftest is testing]]
   [orchard.clojuredocs :as docs]
   [orchard.meta :as sut]))

(deftest merge-meta-test
  (testing "Always safe and preserves object"
    (are [form] (let [x form]
                  (= x (sut/merge-meta x {:random 'meta})))
      0 1 1.0 (float 1) (double 1) 1M 1N 1/2
      'symbol :keyword
      (atom 10) (delay 10)
      [1 2 3] '(1 2 3)
      {1 2} #{1 2 3}))
  (testing "Applies meta"
    (are [form] (-> (sut/merge-meta form {:random 'meta})
                    meta :random (= 'meta))
      ;; Keywords and numbers have no metadata.
      ;; Atoms and delays are mutable.
      'symbol
      [1 2 3] '(1 2 3)
      {1 2} #{1 2 3})))

(deftest strip-meta-test
  (testing "Always safe and preserves object"
    (are [form] (let [x form]
                  (= x (sut/strip-meta (sut/merge-meta x {:random 'meta}))))
      0 1 1.0 (float 1) (double 1) 1M 1N 1/2
      'symbol :keyword
      (atom 10) (delay 10)
      [1 2 3] '(1 2 3)
      {1 2} #{1 2 3}))
  (testing "Removes meta"
    (are [form] (-> (with-meta form {:random 'meta})
                    sut/strip-meta meta :random not)
      ;; Keywords and numbers have no metadata.
      ;; Atoms and delays are mutable.
      'symbol
      [1 2 3] '(1 2 3)
      {1 2} #{1 2 3})))

(defmacro test-macro [& x]
  `(do ~@x))

(deftest macroexpand-all-test
  (is (->> (sut/macroexpand-all '(test-macro ^{:random meta} (hi)))
           second
           meta
           :random
           (= 'meta))))

(deftest special-sym-meta-test
  (testing "Names are correct for `&`, `catch`, `finally`"
    (is (= '& (:name (sut/special-sym-meta '&))))
    (is (= 'catch (:name (sut/special-sym-meta 'catch))))
    (is (= 'finally (:name (sut/special-sym-meta 'finally)))))

  (testing ":see-also metadata is attached"
    (is (not-empty (:see-also (sut/special-sym-meta 'if)))))

  (testing "Name is correct for `clojure.core/import*`"
    ;; Only compiler special to be namespaced
    (is (= 'clojure.core/import* (:name (sut/special-sym-meta 'clojure.core/import*)))))

  (testing "No ns for &, which uses fn's info"
    (is (nil? (:ns (sut/special-sym-meta '&)))))

  (testing "Returns nil for unknown symbol"
    (is (nil? (sut/special-sym-meta 'unknown)))))

(deftest special-sym-meta-without-see-also-test
  (with-redefs [;; do not load documents
                docs/load-docs-if-not-loaded! (constantly nil)]
    (docs/clean-cache!)
    (testing "Attaching see-also is skipped"
      (is (empty? (:see-also (sut/special-sym-meta 'if)))))))

#_{:clj-kondo/ignore [:unused-binding]}
(defn proxied
  "Docstring"
  {:style/indent 1}
  ([])
  ([a b c]))

(def proxy-by-var-quote #'proxied)

(def proxy-by-var-symbol proxied)

(deftest var-meta-test
  ;; Test files can't be found on the class path.
  (is (:file (sut/var-meta #'sut/var-meta)))
  (testing "Includes spec information"
    (is (or (contains? (sut/var-meta (resolve 'let)) :spec)
            (nil? (resolve 'clojure.spec.alpha/spec)))))
  (testing "Includes see-also information from clojure docs"
    (is (contains? (sut/var-meta (resolve 'clojure.set/union)) :see-also)))
  (is (re-find #"string\.clj"
               (:file (#'sut/maybe-add-file
                       {:ns (find-ns 'clojure.string)}))))
  (is (not (re-find #"/form-init[^/]*$"
                    (:file (sut/var-meta
                            (eval '(do (in-ns 'clojure.string)
                                       (def pok 10))))))))

  (testing "Uses logic from `merge-meta-from-proxied-var-clj`"
    (is (= {:doc "Docstring"
            :arglists '([] [a b c])
            :style/indent 1}
           (select-keys (sut/var-meta #'proxy-by-var-quote)
                        [:doc :arglists :style/indent])))
    (is (= {:doc "Docstring"
            :arglists '([] [a b c])
            :style/indent 1}
           (select-keys (sut/var-meta #'proxy-by-var-symbol)
                        [:doc :arglists :style/indent])))))

(deftest merge-meta-from-proxied-var-clj-test
  (testing "Copies `:doc`, `:style/indent` and `:arglist` metadata from the proxied var to the proxy var"
    (testing "For a var which value is var"
      (is (= {:doc "Docstring"
              :arglists '([] [a b c])
              :style/indent 1}
             (select-keys (#'sut/merge-meta-from-proxied-var-clj (meta #'proxy-by-var-quote) #'proxy-by-var-quote)
                          [:doc :arglists :style/indent]))))

    (testing "For a var which value is an object, which at read-time is expressed as a single symbol"
      (is (= {:doc "Docstring"
              :style/indent 1
              :arglists '([] [a b c])}
             (select-keys (#'sut/merge-meta-from-proxied-var-clj (meta #'proxy-by-var-symbol) #'proxy-by-var-symbol)
                          [:doc :arglists :style/indent]))))))

(deftest var-meta-without-see-also-test
  (with-redefs [;; do not load documents
                docs/load-docs-if-not-loaded! (constantly nil)]
    (docs/clean-cache!)
    (testing "Including see-also is skipped"
      (is (not (contains? (sut/var-meta (resolve 'clojure.set/union)) :see-also))))))

(deftest ns-file-test
  (testing "Resolves the file path"
    (let [nss '[orchard.test-ns-dep orchard.test-no-defs]
          endings ["test_ns_dep.cljc" "test_no_defs.cljc"]]
      (is (every? true? (map #(.endsWith (sut/ns-file %1) %2) nss endings))))))

(deftest ns-meta-test
  (let [ns 'orchard.test-ns-dep
        ns-meta (sut/ns-meta ns)]
    (testing "Includes correct `:ns`"
      (is (= ns (:ns ns-meta))))
    (testing "Includes correct `:name`"
      (is (= ns (:name ns-meta))))
    (testing "Includes correct `:file`"
      (is (= (sut/ns-file ns) (:file ns-meta))))
    (testing "Includes `:line 1`"
      (is (= 1 (:line ns-meta))))
    (testing "Does not include anything else"
      (is (= 4 (count (keys ns-meta)))
          (pr-str ns-meta)))))
