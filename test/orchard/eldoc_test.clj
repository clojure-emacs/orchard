(ns orchard.eldoc-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [orchard.eldoc :as eldoc]
   [orchard.info :as info]
   [orchard.test.util :refer [is+]]))

(deftest test-eldoc
  (testing "arglist extraction"
    (is+ {:eldoc '(["x"] ["x" "y"])}
         (eldoc/eldoc {:arglists '([x] [x y])}))
    (is+ {:eldoc '([] ["x"] ["x" "y" "z"])}
         (eldoc/eldoc {:candidates '{X {:arglists ([x])}
                                     Y {:arglists ([x] [x y z])}
                                     Z {:arglists ([])}}}))
    (is+ {:eldoc '([".instanceMember" "instance" "args*"]
                   [".instanceMember" "Classname" "args*"]
                   ["Classname/staticMethod" "args*"]
                   ["Classname/staticField"])}
         (eldoc/eldoc {:forms ['(.instanceMember instance args*)
                               '(.instanceMember Classname args*)
                               '(Classname/staticMethod args*)
                               'Classname/staticField]
                       :special-form true}))

    ;; sanity checks and special cases
    (is (:eldoc (eldoc/eldoc (info/info 'clojure.core 'map))))
    (is (:eldoc (eldoc/eldoc (info/info 'clojure.core '.toString))))
    (is (:eldoc (eldoc/eldoc (info/info 'clojure.core '.))))
    (is (nil? (:eldoc (eldoc/eldoc (info/info 'clojure.core 'non-existing))))))

  (testing "Clojure result structure"
    (is+ {:ns some?
          :name some?
          :type some?
          :eldoc some?
          :docstring some?}
         (eldoc/eldoc (info/info 'clojure.core 'map))))

  (testing "Clojure special form"
    (is+ {:type "special-form"}
         (eldoc/eldoc (info/info 'clojure.core 'if))))

  (testing "Clojure macro"
    (is+ {:type "macro"}
         (eldoc/eldoc (info/info 'clojure.core 'when))))

  (testing "Clojure function"
    (is+ {:type "function"}
         (eldoc/eldoc (info/info 'clojure.core 'inc))))

  (testing "Java result structure"
    (is+ {:class some?
          :member some?
          :type some?
          :eldoc some?}
         (eldoc/eldoc (info/info-java 'java.lang.String 'toLowerCase)))))

;;;; eldoc datomic query
(def testing-datomic-query '[:find ?x
                             :in $ ?name
                             :where
                             [?x :person/name ?name]])

(deftest datomic-query-test
  (testing "eldoc of inline datomic query"
    (is+ {:inputs '(["$" "%" "?person-id"])}
         (eldoc/datomic-query "user" "'[:find ?x :in $ % ?person-id]")))

  (testing "eldoc of inline datomic query as map"
    (is+ {:inputs '(["$" "%" "?person-id"])}
         (eldoc/datomic-query "user" "'{:find [?x] :in [$ % ?person-id]}")))

  (testing "eldoc of datomic query defined as symbol"
    (is+ {:inputs '(["$" "?name"])}
         (eldoc/datomic-query "orchard.eldoc-test" "testing-datomic-query")))

  (testing "eldoc of inline datomic query without :in"
    (is+ {:inputs '(["$"])}
         (eldoc/datomic-query "user" "'[:find ?x]")))

  (testing "eldoc of inline datomic query as map without :in"
    (is+ {:inputs '(["$"])}
         (eldoc/datomic-query "user" "'{:find ?x}"))))
