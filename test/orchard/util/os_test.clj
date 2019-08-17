(ns orchard.util.os-test
  (:require
   [clojure.string :as str]
   [clojure.test :as test :refer [deftest is testing]]
   [orchard.util.os :as os]))

(deftest cache-dir-test
  (testing "BSD"
    (with-redefs [os/os-type ::os/bsd]
      (is (str/ends-with? (os/cache-dir) "/.cache"))))

  (testing "Linux"
    (with-redefs [os/os-type ::os/linux]
      (is (str/ends-with? (os/cache-dir) "/.cache"))))

  (testing "Mac"
    (with-redefs [os/os-type ::os/mac]
      (is (str/ends-with? (os/cache-dir) "/Library/Caches")))))

;; NOTE: This test case targets AppVeyor mainly
(deftest cache-dir-windows-test
  (when (= ::os/windows os/os-type)
    (is (re-seq #"^C:\\Users\\.+\\AppData\\Local" (os/cache-dir)))))
