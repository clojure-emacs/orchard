(ns orchard.util.os-test
  (:require
   [clojure.string :as string]
   [clojure.test :as test :refer [deftest is testing]]
   [orchard.util.os :as os]))

(when-not (= os/os-type ::os/windows)
  (deftest cache-dir-test
    (testing "BSD"
      (with-redefs [os/os-type ::os/bsd]
        (is (string/ends-with? (os/cache-dir) "/.cache"))))

    (testing "Linux"
      (with-redefs [os/os-type ::os/linux]
        (is (string/ends-with? (os/cache-dir) "/.cache"))))

    (testing "Mac"
      (with-redefs [os/os-type ::os/mac]
        (is (string/ends-with? (os/cache-dir) "/Library/Caches"))))))

(when (= os/os-type ::os/windows)
  (deftest cache-dir-windows-test
    (is (re-seq #"^C:\\Users\\.+\\AppData\\Local" (os/cache-dir)))))
