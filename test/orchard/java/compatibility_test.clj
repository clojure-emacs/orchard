(ns orchard.java.compatibility-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [orchard.java.compatibility :as compat]
   [orchard.misc :as misc])
  (:import
   (orchard.java PrivateFieldClass)))

;; The module-system behavior only exists on JDK11+; on JDK8 `module-name`
;; always returns nil, `is-in-boot-module?` is always falsy and JDK-internal
;; fields are freely accessible, so those tests are gated.

(when (>= misc/java-api-version 11)
  (deftest module-name-test
    (testing "JDK classes report the module they belong to"
      (is (= "java.base" (compat/module-name String)))
      (is (= "java.sql" (compat/module-name java.sql.Connection))))
    (testing "symbols are resolved to classes"
      (is (= "java.base" (compat/module-name 'java.lang.String))))
    (testing "classpath classes live in the unnamed module which has no name"
      (is (nil? (compat/module-name PrivateFieldClass)))
      (is (nil? (compat/module-name clojure.lang.PersistentVector)))))

  (deftest is-in-boot-module?-test
    (testing "java.base classes belong to the boot module layer"
      (is (true? (compat/is-in-boot-module? String)))
      (is (true? (compat/is-in-boot-module? java.util.List))))
    (testing "classpath classes don't"
      (is (false? (compat/is-in-boot-module? PrivateFieldClass)))
      (is (false? (compat/is-in-boot-module? clojure.lang.PersistentVector)))))

  (deftest access-denied-test
    (testing "returns ::access-denied for inaccessible JDK internals"
      (let [field (.getDeclaredField String "value")]
        (is (= ::compat/access-denied
               (compat/get-field-value field "hello")))))))

(deftest get-field-value-test
  (testing "reads private fields of classpath classes"
    (let [field (.getDeclaredField PrivateFieldClass "age")]
      (is (= 42 (compat/get-field-value field (PrivateFieldClass. 42))))))
  (testing "reads accessible public fields"
    (let [field (.getField Integer "MAX_VALUE")]
      (is (= Integer/MAX_VALUE (compat/get-field-value field nil))))))
