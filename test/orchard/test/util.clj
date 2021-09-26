(ns orchard.test.util)

(def has-enriched-classpath?
  (let [v (System/getProperty "orchard.internal.has-enriched-classpath")]
    (assert (#{"true" "false"} v))
    (= "true" v)))
