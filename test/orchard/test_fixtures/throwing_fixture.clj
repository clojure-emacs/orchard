(ns orchard.test-fixtures.throwing-fixture
  "A namespace whose once-fixture throws, run by `orchard.test-test`."
  (:require
   [clojure.test :refer [deftest is use-fixtures]]))

(defn- throwing-fixture [_]
  (throw (ex-info "fixture failed" {:data 42})))

(use-fixtures :once throwing-fixture)

(deftest never-runs
  (is true))
