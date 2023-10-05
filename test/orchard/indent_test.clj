(ns orchard.indent-test
  (:require
   [clojure.test :refer [are deftest is testing]]
   [orchard.indent :as sut]))

(deftest clojure-mode-indents
  (let [extract-keys (fn [m]
                       (->> m
                            vals
                            (map first)
                            (remove (fn [x]
                                      (and (symbol? x)
                                           (-> x symbol namespace)
                                           ;; these complicate the CI matrix, since c.a is not available in older Clojures:
                                           (or (-> x namespace (= "clojure.core.async"))
                                               (-> x namespace (= "clojure.spec.alpha"))))))
                            set))
        exact (extract-keys sut/clojure-mode-indents-exact)
        fuzzy (extract-keys sut/clojure-mode-indents-fuzzy)]
    (assert (contains? exact 'clojure.test/are))
    (assert (contains? exact 'catch))
    (assert (contains? exact 'clojure.core/->))
    (assert (contains? fuzzy 'clojure.core/defprotocol))
    (assert (contains? fuzzy 'finally))
    (testing "There are no indents defined twice"
      (doseq [x exact]
        (is (not (contains? fuzzy x))))
      (doseq [x fuzzy]
        (is (not (contains? exact x)))))

    (doseq [x (into exact fuzzy)]
      (is (or (and (symbol? x)
                   (not (namespace x)))
              (and (symbol? x)
                   (namespace x)
                   (#'sut/try-requiring-resolve x)))
          (str x " denotes an existing var")))))

(deftest compute-style-indent-test
  (are [macro-name arglists expected] (testing [macro-name arglists]
                                        (is (= expected
                                               (#'sut/compute-style-indent (str macro-name) arglists)))
                                        true)
    #_macro-name #_arglists              #_expected

    ;; parsing based on the macro name:

    'defprotocol '[[name & opts+sigs]]   [1 [:defn]]
    ;; structurally equivalent:
    'defprotocol '[[nAme & Opts+siGs]]   [1 [:defn]]
    ;; structurally different (`&` is special, so the 3 elements are deemed different):
    'defprotocol '[[a b c]]              nil
    ;; structurally different:
    'defprotocol '[[something-else]]     nil
    ;; name is a fuzzy match of defprotocol:
    'defprotocolA '[[name & opts+sigs]]  [1 [:defn]]
    ;; name is not a fuzzy match of defprotocol, so gets a different value:
    'dfprotocol '[[name & opts+sigs]]    1
    ;; gets the value defined for the special form `catch`:
    'catch      '[]                      2
    ;; Does not get the value defined for the special form `catch` (it's defined in `sut/clojure-mode-indents-exact`):
    'catcha     '[]                      nil
    ;; Gets the value defined for the special form `finally`:
    'finally    '[]                      0
    ;; Gets the value defined for the special form `finally` (it's defined in `sut/clojure-mode-indents-fuzzy`):
    'finallyA   '[]                      0

    ;; parsing based on `&`:

    'anything    '[[& body]]             0
    'anything    '[[a & body]]           1
    'anything    '[[a {} & body]]        2
    'anything    '[[{} & body]]          1
    'anything    '[[{} a & body]]        2
    ;; macros starting by 'def' get no inference:
    'defanything '[[a & body]]           nil
    'defanything '[[a {} & body]]        nil
    'defanything '[[{} & body]]          nil
    'defanything '[[{} a & body]]        nil
    ;; un-inferrable:
    'anything    '[[a & body], [& body]] nil

    ;; parsing based on argument names:

    'anything    '[[foo]]                nil
    'anything    '[[body]]               0
    'anything    '[[a body]]             1
    'anything    '[[a b body]]           2
    ;; macros starting by 'def' get no inference:
    'defanything '[[body]]               nil
    'defanything '[[a body]]             nil
    'defanything '[[a b body]]           nil
    ;; un-inferrable:
    'anything    '[[a b body]
                   [a b C body]]         nil
    ;; condition-like macros get inference:
    'sdfds       '[[condition
                    then
                    else]]               1
    ;; condition-like macros starting by 'def' get no inference:
    'defnfoo     '[[condition
                    then
                    else]]               nil

    ;; Threading forms:
    '->          '[[x & forms]]          nil
    '->>         '[[x & forms]]          nil
    'some->      '[[x & forms]]          nil
    'some->>     '[[x & forms]]          nil))
