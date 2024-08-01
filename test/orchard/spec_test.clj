(ns orchard.spec-test
  (:require
   [clojure.spec.alpha :as spec1]
   [clojure.test :refer [deftest is testing]]
   [orchard.spec :as spec]))

(deftest normalize-spec-form-test
  (testing "All fn* subforms should be normalized"
    (is (= (spec/normalize-spec-form '(clojure.spec.alpha/fspec
                                       :args (clojure.spec.alpha/and
                                              (fn* [p1__22097#]
                                                   (clojure.core/< (:start p1__22097#) (:end p1__22097#)))
                                              (clojure.core/fn [%]
                                                (clojure.core/< (:start %) (:end %))))
                                       :ret (fn* [p2__33098#]
                                                 (clojure.core/> (:start p2__33098#) (:end p2__33098#)))
                                       :fn nil))
           '(clojure.spec.alpha/fspec
             :args (clojure.spec.alpha/and
                    (clojure.core/fn [%] (clojure.core/< (:start %) (:end %)))
                    (clojure.core/fn [%] (clojure.core/< (:start %) (:end %))))
             :ret (clojure.core/fn [%] (clojure.core/> (:start %) (:end %)))
             :fn nil)))))

(deftest spec-is-found-by-ns-alias
  (testing "current ns keyword"
    (testing "spec-list finds current ns keyword"
      (spec1/def ::foo string?)
      (let [specs (set (spec/spec-list "" "orchard.spec-test"))]
        (is (specs "::foo") "Spec is found with current ns")
        (is (specs ":orchard.spec-test/foo") "Spec is found with fully qualified name")))

    (testing "spec-form finds current ns keyword"
      (let [spec1 (spec/spec-form "::foo" "orchard.spec-test")
            spec2 (spec/spec-form ":orchard.spec-test/foo" "orchard.spec-test")]
        (is (= "clojure.core/string?" spec1 spec2) "Both return the same correct spec"))))

  (testing "ns aliased keyword"
    (spec1/def :orchard.spec/test-dummy boolean?)
    (testing "spec-list finds keyword in aliased namespace"

      (let [specs (set (spec/spec-list "" "orchard.spec-test"))]
        (is (specs "::spec/test-dummy") "Spec is found with ns-aliased keyword")
        (is (specs ":orchard.spec/test-dummy") "Spec is found with fully qualified name")))

    (testing "spec-form finds keyword in aliased namespace"
      (let [spec1 (spec/spec-form "::spec/test-dummy" "orchard.spec-test")
            spec2 (spec/spec-form ":orchard.spec/test-dummy" "orchard.spec-test")]
        (is (= "clojure.core/boolean?" spec1 spec2) "Both return the same correct spec")))))

(when spec/clojure-alpha-spec?
  (deftest spec-registry-precedence-test
    (testing "a spec registered in multiple registries"
      (eval '(clojure.spec.alpha/def ::bar string?))
      (eval '(clojure.alpha.spec/def ::bar boolean?))
      (is (= "clojure.core/boolean?" (spec/spec-form "::bar" "orchard.spec-test"))
          "should be resolved from the spec-2 registry"))))
