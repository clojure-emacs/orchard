(ns orchard.clojuredocs-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :as test :refer [deftest is testing use-fixtures]]
   [orchard.clojuredocs :as docs])
  (:import
   (java.time Instant)))

(def ^:private test-edn-file
  (io/resource "clojuredocs/test_export.edn"))

(def ^:private now
  (-> (Instant/now) .getEpochSecond (* 1000)))

(defn- create-dummy-cache-file [& [timestamp]]
  (let [cache-file docs/cache-file]
    (.. cache-file
        getParentFile
        mkdirs)
    (doto cache-file
      (spit (slurp test-edn-file))
      (cond-> timestamp (.setLastModified timestamp)))))

(defn- clojuredocs-test-fixture [f]
  (with-redefs [docs/cache-file (io/file "target/clojuredocs/export.edn")]
    (docs/clean-cache!)
    (f)
    (docs/clean-cache!)))

(use-fixtures :each clojuredocs-test-fixture)

(deftest load-docs-if-not-loaded!-test
  (let [cache-file docs/cache-file]
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
  (let [cache-file docs/cache-file]
    (testing "accessible to remote export.edn"
      (is (not (.exists cache-file)))
      (is (empty? @docs/cache))
      (docs/update-cache! test-edn-file)
      (is (.exists cache-file))
      (is (contains? @docs/cache :foo.core/bar))
      (docs/clean-cache!))

    (testing "not accessible to remote export.edn"
      (is (not (.exists cache-file)))
      (is (empty? @docs/cache))
      (is (thrown? Exception (docs/update-cache! "http://example.com/no/such/file.edn")))
      (is (not (.exists cache-file)))
      (is (empty? @docs/cache))
      (is (thrown? Exception (docs/update-cache! "http://non.existing.server/no/such/file.edn")))
      (is (not (.exists cache-file)))
      (is (empty? @docs/cache)))))

(deftest update-cache!-non-existing-url-test
  (let [cache-file docs/cache-file]
    (is (not (.exists cache-file)))
    (is (empty? @docs/cache))
    (is (thrown? Exception (docs/update-cache! "file:/not/existing/file.edn")))
    (is (not (.exists cache-file)))
    (is (empty? @docs/cache))))

(deftest update-cache!-existing-cache-file-test
  (let [cache-file docs/cache-file]
    (testing "no cached documentation"
      (create-dummy-cache-file now)
      (reset! docs/cache {})

      (is (= now (.lastModified cache-file)))
      (is (empty? @docs/cache))
      (docs/update-cache! test-edn-file)
      ;; should be updated
      (is (not= now (.lastModified cache-file)))
      (is (contains? @docs/cache :foo.core/bar)))

    (testing "not accessible to remote export.edn"
      (docs/clean-cache!)
      (create-dummy-cache-file now)

      (is (= now (.lastModified cache-file)))
      (is (empty? @docs/cache))
      (is (thrown? Exception (docs/update-cache! "bad/file.edn")))
      (is (= now (.lastModified cache-file)))
      (is (empty? @docs/cache)))))

(deftest clean-cache!-test
  (create-dummy-cache-file)
  (reset! docs/cache {:dummy "not-empty-dummy-data"})
  (let [cache-file docs/cache-file]
    (is (.exists cache-file))
    (is (seq @docs/cache))
    (docs/clean-cache!)
    (is (not (.exists cache-file)))
    (is (empty? @docs/cache))))

(deftest find-doc-test
  (testing "find existing documentation"
    (is (empty? @docs/cache))
    (let [result (docs/find-doc "clojure.core" "first")]
      (is (map? result))
      (is (every? #(contains? result %)
                  [:arglists :doc :examples :name :notes :ns :see-alsos]))
      (is (seq @docs/cache))))

  (testing "find non-existing documentation"
    (is (nil? (docs/find-doc "non-existing-ns" "non-existing-var")))))
