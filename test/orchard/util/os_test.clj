(ns orchard.util.os-test
  (:require
   [clojure.test :as test :refer [deftest testing]]
   [orchard.test.util :refer [is+]]
   [orchard.util.os :as os]))

(when-not (= os/os-type ::os/windows)
  (deftest cache-dir-test
    (testing "BSD"
      (with-redefs [os/os-type ::os/bsd]
        (is+ #"/\.cache$" (str (os/cache-dir)))))

    (testing "Linux"
      (with-redefs [os/os-type ::os/linux]
        (is+ #"/\.cache$" (str (os/cache-dir)))))

    (testing "Mac"
      (with-redefs [os/os-type ::os/mac]
        (is+ #"/Library/Caches$" (str (os/cache-dir)))))))

(when (= os/os-type ::os/windows)
  (deftest cache-dir-windows-test
    (is+ #"^C:\\Users\\.+\\AppData\\Local\\Cache" (str (os/cache-dir)))))
