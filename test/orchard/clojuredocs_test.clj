(ns orchard.clojuredocs-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :as test :refer [deftest is testing use-fixtures]]
   [orchard.clojuredocs :as docs])
  (:import
   (java.io FileNotFoundException IOException)
   (java.time Instant)))

(def ^:private test-edn-file
  (io/resource "clojuredocs/export.edn"))

(def ^:private now
  (-> (Instant/now) .getEpochSecond (* 1000)))

(def ^:private new-timestamp
  (- now (/ docs/cache-updating-threshold 2)))

(def ^:private old-timestamp
  (- now docs/cache-updating-threshold))

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

(deftest load-cache!-no-cache-file-test
  (let [cache-file (io/file docs/cache-file-name)]
    (testing "accessible to remote export.edn"
      (with-redefs [docs/test-remote-url (constantly [true])]
        (is (not (.exists cache-file)))
        (is (empty? @docs/cache))
        (docs/load-cache! test-edn-file)
        (is (.exists cache-file))
        (is (not (empty? @docs/cache)))
        (docs/clean-cache!)))

    (testing "not accessible to remote export.edn"
      (with-redefs [docs/test-remote-url (constantly [false (IOException. "dummy")])]
        (is (not (.exists cache-file)))
        (is (empty? @docs/cache))
        (is (thrown? IOException (docs/load-cache! test-edn-file)))
        (is (not (.exists cache-file)))
        (is (empty? @docs/cache))))))

(deftest load-cache!-non-existing-url-test
  (let [cache-file (io/file docs/cache-file-name)]
    (is (not (.exists cache-file)))
    (is (empty? @docs/cache))
    (is (thrown? FileNotFoundException (docs/load-cache! "file:/not/existing/file.edn")))
    (is (not (.exists cache-file)))
    (is (empty? @docs/cache))))

(deftest load-cache!-old-cache-file-test
  (let [cache-file (io/file docs/cache-file-name)]
    (testing "accessible to remote export.edn"
      (with-redefs [docs/test-remote-url (constantly [true])]
        (create-dummy-cache-file old-timestamp)
        (reset! docs/cache {:dummy "not-empty-dummy-data"})

        (is (= old-timestamp (.lastModified cache-file)))
        (is (contains? @docs/cache :dummy))
        (docs/load-cache! test-edn-file)
        (is (< old-timestamp (.lastModified cache-file)))
        (is (not (contains? @docs/cache :dummy)))))

    (testing "not accessible to remote export.edn"
      (with-redefs [docs/test-remote-url (constantly [false (IOException. "dummy")])]
        (create-dummy-cache-file old-timestamp)
        (reset! docs/cache {})

        (is (= old-timestamp (.lastModified cache-file)))
        (is (empty? @docs/cache))
        ;; Use existing cache file
        (docs/load-cache! test-edn-file)
        (is (= old-timestamp (.lastModified cache-file)))
        (is (seq @docs/cache))))))

(deftest load-cache!-sufficiently-new-cache-file-test
  (let [cache-file (io/file docs/cache-file-name)]
    (testing "no cached documentation"
      (with-redefs [docs/test-remote-url (constantly [true])]
        (create-dummy-cache-file new-timestamp)
        (reset! docs/cache {})

        (is (= new-timestamp (.lastModified cache-file)))
        (is (empty? @docs/cache))
        (docs/load-cache! test-edn-file)
        (is (= new-timestamp (.lastModified cache-file)))
        (is (not (empty? @docs/cache)))))

    (testing "already cached documentation"
      (with-redefs [docs/test-remote-url (constantly [true])]
        (create-dummy-cache-file new-timestamp)
        (reset! docs/cache {:dummy "not-empty-dummy-data"})

        (is (= new-timestamp (.lastModified cache-file)))
        (is (contains? @docs/cache :dummy))
        (docs/load-cache! test-edn-file) ; Does nothing

        (is (= new-timestamp (.lastModified cache-file)))
        (is (contains? @docs/cache :dummy))))

    (testing "not accessible to remote export.edn"
      (with-redefs [docs/test-remote-url (constantly [false (IOException. "dummy")])]
        (create-dummy-cache-file new-timestamp)
        (reset! docs/cache {})

        (is (= new-timestamp (.lastModified cache-file)))
        (is (empty? @docs/cache))
        ;; Use existing cache file
        (docs/load-cache! test-edn-file)
        (is (= new-timestamp (.lastModified cache-file)))
        (is (not (empty? @docs/cache)))))))

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
