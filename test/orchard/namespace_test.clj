(ns orchard.namespace-test
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer :all]
   [orchard.classpath :as cp]
   [orchard.misc :as misc]
   [orchard.namespace :as n]))

(deftest project-namespaces-test
  (is (contains? (into #{} (n/project-namespaces))
                 'orchard.namespace)))

(deftest loaded-namespaces-test
  ;; If we don't pass the second arg, some cider ns will be returned
  (is (some #(re-find #".*orchard" %) (n/loaded-namespaces)))
  ;; Shouldn't return any orchard namespaces
  (is (not-any? #(re-find #".*orchard" %)
                (n/loaded-namespaces [".*orchard"]))))

(defn- change-case
  "Utility fn to change the case of a string, to help with case sensitivity
  tests that follows"
  [string]
  (let [upper (str/upper-case string)
        lower (str/lower-case string)]
    (if (= string lower) upper lower)))

(deftest project-nses-ignore-case-on-windows-test
  (let [orig-project-root n/project-root]
    (testing "Project nses is case sensitive on non Windows oses"
      (with-redefs [misc/os-windows? (constantly false)
                    n/project-root   (change-case orig-project-root)]
        (is (not (seq (n/project-namespaces))))))
    (testing "Project nses ignore cases on Windows oses"
      (with-redefs [misc/os-windows? (constantly true)
                    n/project-root   (change-case orig-project-root)]
        (is (seq (n/project-namespaces)))))))

(deftest has-tests-errors
  (is (n/has-tests? (find-ns 'orchard.namespace-test))))

(deftest namespace-parsing
  (testing "Namespace parsing"
    (let [url (-> (System/getProperty "java.io.tmpdir")
                  (io/file "orchard.namespace-test.txt")
                  (io/as-url))
          uri (.toURI url)]
      (testing "of an empty file"
        (spit url "")
        (is (nil? (n/read-namespace uri))))
      (testing "of an unparsable file"
        (spit url "(]$@(")
        (is (nil? (n/read-namespace uri))))
      (testing "of non-list tokens"
        (spit url "these are (still) tokens")
        (is (nil? (n/read-namespace uri))))
      (testing "when tokens precede the ns form"
        (spit url "there [is a] (ns here) after all")
        (is (= (n/read-namespace uri) 'here)))
      (testing "when multiple ns forms are present"
        (spit url "(ns ns1) (ns ns2) (ns ns3)")
        (is (= (n/read-namespace uri) 'ns1)))
      (testing "of top-level forms only"
        (spit url "(comment (ns ns1)) (ns ns2) (ns ns3)")
        (is (= (n/read-namespace uri) 'ns2)))
      (io/delete-file url))))

(deftest namespace-resolution
  (testing "Resolving"
    (let [nses '[clojure.java.io
                 clojure.string
                 clojure.test
                 orchard.misc
                 orchard.namespace]]
      (testing "namespace symbols to source files"
        (is (every? identity (map n/canonical-source nses))))
      (testing "source files to namespace symbols"
        (is (= nses (map (comp n/read-namespace    ; src -> ns
                               n/canonical-source) ; ns -> src
                         nses)))))))
