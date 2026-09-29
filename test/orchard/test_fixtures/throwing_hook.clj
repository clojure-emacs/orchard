(ns orchard.test-fixtures.throwing-hook
  "A namespace whose `test-ns-hook` throws outside of an assertion, run by
  `orchard.test-test`.")

(defn test-ns-hook []
  (throw (ex-info "hook failed" {})))
