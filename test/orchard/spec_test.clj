(ns orchard.spec-test
  (:require
   [clojure.test :refer :all]
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
