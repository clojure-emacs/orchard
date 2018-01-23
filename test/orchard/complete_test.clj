(ns orchard.complete-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [compliment.utils]
            [orchard.complete :as sut]))

(deftest complete-test
  (testing "blank"
    (let [cs (sut/complete {:ns "user" :symbol ""})]
      (is (sequential? cs))
      (is (every? map? cs))))

  (testing "basic usage"
    (let [cs (sut/complete {:ns "user"
                            :symbol "filt"})]
      (is (= #{"filter" "filterv"} (set (map :candidate cs))))

      (is (= #{"clojure.core"} (set (map :ns cs))))))

  (testing "function arglists"
    (let [cs (sut/complete {:ns "user"
                            :symbol "unchecked-a"
                            :extra-metadata ["arglists"]})]
      (is (= {:candidate "unchecked-add"
              :type :function
              :ns "clojure.core"
              :arglists '("[x y]")}
             (first cs)))))

  (testing "function metadata"
    (let [cs (sut/complete {:ns "user"
                            :symbol "assoc"
                            :extra-metadata ["arglists" "doc"]})
          {:keys [arglists doc]} (first cs)]
      (is (= '("[map key val]" "[map key val & kvs]") arglists))
      (is (string? doc))))

  (testing "macro metadata"
    (let [cs (sut/complete {:ns "user"
                            :symbol "defprot"
                            :extra-metadata ["arglists" "doc"]})
          {:keys [arglists doc type]} (first cs)]
      (is (= :macro type))
      (is (= '("[name & opts+sigs]") arglists))
      (is (string? doc)))))

(deftest complete-doc-test
  (testing "blank"
    (let [doc (sut/complete-doc {:symbol ""})]
      (is (= "" doc))))

  (testing "basic usage"
    (let [doc (sut/complete-doc {:symbol "true?"})]
      (is (str/starts-with? doc  "clojure.core/true?\n([x")))))
