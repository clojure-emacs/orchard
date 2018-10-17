(ns orchard.query-test
  (:require
   [clojure.set :as set]
   [clojure.test :refer :all]
   [orchard.query :refer :all]))

(def ^:private a-private)
(def ^:abc a-metad)
(def ^:abc ^:def b-metad)

(defn- docd-fn
  "@@@regexpfriendly"
  [])

(deftest namespaces-test
  (are [ns-query subset] (set/subset?
                          (set (map the-ns subset))
                          (set (namespaces ns-query)))
    {:has-tests? true} #{'orchard.namespace-test}
    {:project? true} #{'orchard.core 'orchard.meta 'orchard.namespace-test}
    {:project? true
     :has-tests? true} #{'orchard.namespace-test 'orchard.query-test}
    {:include-regexps [#"query"]} #{'orchard.query 'orchard.query-test}
    {:include-regexps [#"query"]
     :exclude-regexps [#"test$"]} #{'orchard.query})
  (are [ns-query subset] (empty? (set/intersection
                                  (set (map the-ns subset))
                                  (set (namespaces ns-query))))
    {:has-tests? true} #{'orchard.namespace}
    {:project? true} #{'clojure.core}
    {:project? true
     :has-tests? true} #{'orchard.namespace 'clojure.core}
    {:include-regexps [#"orchard"]} #{'clojure.core}
    {:include-regexps [#"orchard" #"query-env"]} #{'clojure.core 'clojure.set}
    {:include-regexps [#"query"]
     :exclude-regexps [#"test$"]} #{'orchard.query-test})
  (is (= (namespaces {:exactly [(the-ns 'clojure.core) (the-ns 'clojure.set)]})
         [(the-ns 'clojure.core) (the-ns 'clojure.set)])))

(deftest vars-test
  (are [var-query subset] (set/subset?
                           (set (map find-var subset))
                           (set (vars var-query)))
    {:exactly [(find-var 'orchard.query-test/a-private)]}
    #{'orchard.query-test/a-private}
    {:private? true} #{'orchard.query-test/a-private
                       'orchard.query/vars}
    {:test? true} #{'orchard.query-test/vars-test}
    {:include-meta-key [:abc]} #{'orchard.query-test/a-metad
                                 'orchard.query-test/b-metad}
    {:exclude-meta-key [:def]} #{'orchard.query-test/a-metad}
    {:search #"metad"} #{'orchard.query-test/a-metad
                         'orchard.query-test/b-metad}
    {:search #"@@@.*"
     :search-property :doc
     :private? true} #{'orchard.query-test/docd-fn})

  (is
   (set/subset?
    #{'x/my-test-var}
    (set
     (vars
      {:manipulate-vars
       (fn [nss vars]
         (concat vars ['x/my-test-var]))}))))

  (are [var-query subset] (empty? (set/intersection
                                   (set (map find-var subset))
                                   (set (vars var-query))))
    {} #{'orchard.query-test/a-private}
    {:test? true} #{'orchard.query/namespaces
                    'orchard.query/vars}
    {:search #"vars-test"} #{'clojure.core/defn 'orchard.query-test/namespaces-test}
    {:include-meta-key [:abc]
     :exclude-meta-key [:def]} #{'orchard.query-test/b-metad}
    {:exclude-meta-key [:def]} #{'orchard.query-test/b-metad}))
