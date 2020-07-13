(ns orchard.clojuredocs-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :as test :refer [deftest is testing use-fixtures]]
   [orchard.clojuredocs :as docs])
  (:import
   (java.io FileNotFoundException IOException)
   (java.time Instant)))

(def ^:private test-edn-file
  (io/resource "clojuredocs/test_export.edn"))

(def ^:private now
  (-> (Instant/now) .getEpochSecond (* 1000)))

(defn- create-dummy-cache-file [& [timestamp]]
  (let [cache-file (io/file docs/cache-file-name)]
    (.. cache-file
        getParentFile
        mkdirs)
    (doto cache-file
      (spit (slurp test-edn-file))
      (cond-> timestamp (.setLastModified timestamp)))))

(defn- clojuredocs-test-fixture [f]
  (with-redefs [docs/cache-file-name "target/clojuredocs/export.edn"]
    (docs/clean-cache!)
    (f)
    (docs/clean-cache!)))

(use-fixtures :each clojuredocs-test-fixture)

(deftest load-docs-if-not-loaded!-test
  (let [cache-file (io/file docs/cache-file-name)]
    (testing "bundled"
      (is (not (.exists cache-file)))
      (is (empty? @docs/cache))
      (docs/load-docs-if-not-loaded!)
      (is (not (.exists cache-file)))
      (is (seq @docs/cache))
      (docs/clean-cache!))

    (testing "cached"
      (create-dummy-cache-file now)
      (is (.exists cache-file))
      (is (empty? @docs/cache))
      (docs/load-docs-if-not-loaded!)
      (is (.exists cache-file))
      (is (contains? @docs/cache :foo.core/bar))
      (docs/clean-cache!))

    (testing "already loaded"
      (reset! docs/cache {::already ::loaded})

      (is (not (.exists cache-file)))
      (is (= {::already ::loaded} @docs/cache))
      (docs/load-docs-if-not-loaded!)
      (is (not (.exists cache-file)))
      (is (= {::already ::loaded} @docs/cache))

      (create-dummy-cache-file now)
      (is (.exists cache-file))
      (is (= {::already ::loaded} @docs/cache))
      (docs/load-docs-if-not-loaded!)
      (is (.exists cache-file))
      (is (= {::already ::loaded} @docs/cache))
      (docs/clean-cache!))))

(deftest update-cache!-no-cache-file-test
  (let [cache-file (io/file docs/cache-file-name)]
    (testing "accessible to remote export.edn"
      (with-redefs [docs/test-remote-url (constantly [true])]
        (is (not (.exists cache-file)))
        (is (empty? @docs/cache))
        (docs/update-cache! test-edn-file)
        (is (.exists cache-file))
        (is (contains? @docs/cache :foo.core/bar))
        (docs/clean-cache!)))

    (testing "not accessible to remote export.edn"
      (with-redefs [docs/test-remote-url (constantly [false (IOException. "dummy")])]
        (is (not (.exists cache-file)))
        (is (empty? @docs/cache))
        (is (thrown? IOException (docs/update-cache! test-edn-file)))
        (is (not (.exists cache-file)))
        (is (empty? @docs/cache))))))

(deftest update-cache!-non-existing-url-test
  (let [cache-file (io/file docs/cache-file-name)]
    (is (not (.exists cache-file)))
    (is (empty? @docs/cache))
    (is (thrown? FileNotFoundException (docs/update-cache! "file:/not/existing/file.edn")))
    (is (not (.exists cache-file)))
    (is (empty? @docs/cache))))

(deftest update-cache!-existing-cache-file-test
  (let [cache-file (io/file docs/cache-file-name)]
    (testing "no cached documentation"
      (with-redefs [docs/test-remote-url (constantly [true])]
        (create-dummy-cache-file now)
        (reset! docs/cache {})

        (is (= now (.lastModified cache-file)))
        (is (empty? @docs/cache))
        (docs/update-cache! test-edn-file)
        ;; should be updated
        (is (not= now (.lastModified cache-file)))
        (is (contains? @docs/cache :foo.core/bar))))

    (testing "not accessible to remote export.edn"
      (with-redefs [docs/test-remote-url (constantly [false (IOException. "dummy")])]
        (create-dummy-cache-file now)
        (reset! docs/cache {})

        (is (= now (.lastModified cache-file)))
        (is (empty? @docs/cache))
        (is (thrown? IOException (docs/update-cache! test-edn-file)))
        (is (= now (.lastModified cache-file)))
        (is (empty? @docs/cache))))))

(deftest clean-cache!-test
  (create-dummy-cache-file)
  (reset! docs/cache {:dummy "not-empty-dummy-data"})
  (let [cache-file (io/file docs/cache-file-name)]
    (is (.exists cache-file))
    (is (not (empty? @docs/cache)))
    (docs/clean-cache!)
    (is (not (.exists cache-file)))
    (is (empty? @docs/cache))))

(deftest find-doc-test
  (testing "find existing documentation"
    (with-redefs [docs/test-remote-url (constantly [true])]
      (is (empty? @docs/cache))
      (is (not (.exists (io/file docs/cache-file-name))))
      (let [result (docs/find-doc "clojure.core" "first" test-edn-file)]
        (is (map? result))
        (is (every? #(contains? result %)
                    [:arglists :doc :examples :name :notes :ns :see-alsos]))
        (is (.exists (io/file docs/cache-file-name)))
        (is (not (empty? @docs/cache))))))

  (testing "find non-existing documentation"
    (is (nil? (docs/find-doc "non-existing-ns" "non-existing-var" test-edn-file)))))
