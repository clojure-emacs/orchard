(ns orchard.util.os-test
  (:require
   [clojure.string :as str]
   [clojure.test :as test :refer [deftest is testing]]
   [orchard.test.util :refer [is+]]
   [orchard.util.os :as os]))

(when-not (= os/os-type ::os/windows)
  (deftest cache-dir-test
    (testing "BSD"
      (with-redefs [os/os-type ::os/bsd]
        (is (str/ends-with? (os/cache-dir) "/.cache"))))

    (testing "Linux"
      (with-redefs [os/os-type ::os/linux]
        (is (str/ends-with? (os/cache-dir) "/.cache"))))

    (testing "Mac"
      (with-redefs [os/os-type ::os/mac]
        (is (str/ends-with? (os/cache-dir) "/Library/Caches"))))))

(when (= os/os-type ::os/windows)
  (deftest cache-dir-windows-test
    (is+ #"^C:\\Users\\.+\\AppData\\Local\\Cache" (str (os/cache-dir)))))
