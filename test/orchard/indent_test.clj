(ns orchard.indent-test
  (:require
   [clojure.set :as set]
   [clojure.test :refer [deftest testing]]
   [matcher-combinators.matchers :as mc]
   [orchard.indent :as sut]
   [orchard.test.util :refer [is+ are*]]))

(deftest clojure-mode-indents
  (let [extract-keys #(->> (vals %) (map first) set)
        exact (extract-keys sut/clojure-mode-indents-exact)
        fuzzy (extract-keys sut/clojure-mode-indents-fuzzy)]
    (is+ (mc/embeds '#{clojure.test/are catch clojure.core/->}) exact)
    (is+ (mc/embeds '#{clojure.core/defprotocol finally}) fuzzy)
    (testing "There are no indents defined twice"
      (is+ empty? (set/intersection exact fuzzy)))))

(deftest compute-style-indent-test
  (are* [macro-name arglists expected] (testing [macro-name arglists]
                                         (is+ expected
                                              (#'sut/compute-style-indent (str macro-name) arglists)))
    #_macro-name #_arglists              #_expected

    ;; parsing based on the macro name:

    'defprotocol '[[name & opts+sigs]]   [[:block 1] [:inner 1]]
    ;; structurally equivalent:
    'defprotocol '[[nAme & Opts+siGs]]   [[:block 1] [:inner 1]]
    ;; structurally different (`&` is special, so the 3 elements are deemed different):
    'defprotocol '[[a b c]]              nil
    ;; structurally different:
    'defprotocol '[[something-else]]     nil
    ;; name is a fuzzy match of defprotocol:
    'defprotocolA '[[name & opts+sigs]]  [[:block 1] [:inner 1]]
    ;; name is not a fuzzy match of defprotocol, so gets a different value:
    'dfprotocol '[[name & opts+sigs]]    1
    ;; gets the value defined for the special form `catch`:
    'catch      '[]                      [[:block 2]]
    ;; Does not get the value defined for the special form `catch` (it's defined in `sut/clojure-mode-indents-exact`):
    'catcha     '[]                      nil
    ;; Gets the value defined for the special form `finally`:
    'finally    '[]                      [[:block 0]]
    ;; Gets the value defined for the special form `finally` (it's defined in `sut/clojure-mode-indents-fuzzy`):
    'finallyA   '[]                      [[:block 0]]

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
