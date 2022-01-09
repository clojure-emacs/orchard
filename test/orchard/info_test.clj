(ns orchard.info-test
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str :refer [replace-first]]
   [clojure.test :refer [are deftest is testing use-fixtures]]
   [orchard.info :as info]
   [orchard.java :as java]
   [orchard.misc :as misc]
   [orchard.test-ns]
   [orchard.test.util :as util]))

@java/cache-initializer ;; make tests more deterministic

(def cljs-available?
  (let [sym 'orchard.cljs.test-env
        fname (-> sym str (str/replace "." "/") (str/replace "-" "_") (str ".cljc"))]
    (assert (some-> fname io/resource io/as-file .exists)
            (format "The %s file can be required to begin with" fname))
    (try
      (require sym)
      true
      (catch Exception _
        false))))

(def ^:dynamic *cljs-params*)

(defn wrap-info-params
  [f]
  (with-bindings (cond-> {}
                   cljs-available? (assoc #'*cljs-params* {:dialect :cljs
                                                           :env (@(resolve 'orchard.cljs.test-env/create-test-env))}))
    (f)))

(use-fixtures :once wrap-info-params)

(deftest info-non-existing-test
  (testing "Non existing symbol in clojure.core"
    (is (nil? (info/info* {:ns 'clojure.core :sym (gensym "non-existing")}))))

  (testing "Non existing symbol in user - issue #86"
    (is (nil? (info/info* {:ns 'user :sym (gensym "non-existing")})))))

(deftest info-deftype-test
  (testing "deftype"
    (when cljs-available?
      (testing "- :cljs"
        (let [i (info/info 'orchard.test-ns 'TestType *cljs-params*)]
          (is (= '{:ns orchard.test-ns
                   :name TestType
                   :type true
                   :record false
                   :tag function
                   :arglists nil}
                 (select-keys i [:ns :name :record :type :tag :arglists])))
          (is (str/includes? (:file i) "test_ns"))))))

  (testing "- :clj"
    (let [i (info/info 'orchard.test-ns 'TestType)]
      (is (= '{:name TestType
               :class orchard.test_ns.TestType
               :package orchard.test_ns
               :super java.lang.Object
               :interfaces (clojure.lang.IType)
               :javadoc "orchard/test_ns/TestType.html"}
             (select-keys i [:ns :name :class :package :super :interfaces :arglists :javadoc :file]))))))

(deftest info-defrecord-test
  (testing "defrecord"
    (when cljs-available?
      (testing "- :cljs"
        (let [i (info/info 'orchard.test-ns 'TestRecord *cljs-params*)]
          (is (= '{:ns orchard.test-ns
                   :name TestRecord
                   :type true
                   :record true
                   :tag function
                   :arglists nil}
                 (select-keys i [:ns :name :record :type :tag :arglists])))
          (is (str/includes? (:file i) "test_ns")))))

    (testing "- :clj"
      (let [i (info/info 'orchard.test-ns 'TestRecord)]
        (is (= '{:name TestRecord
                 :class orchard.test_ns.TestRecord
                 :package orchard.test_ns
                 :super java.lang.Object
                 :interfaces (clojure.lang.IRecord
                              clojure.lang.IHashEq
                              clojure.lang.IObj
                              clojure.lang.ILookup
                              clojure.lang.IKeywordLookup
                              clojure.lang.IPersistentMap
                              java.util.Map
                              java.io.Serializable)
                 :javadoc "orchard/test_ns/TestRecord.html"}
               (select-keys i [:ns :name :class :package :super :interfaces :arglists :javadoc :file])))))))

(deftest info-special-form-test
  (testing "special forms are marked as such and nothing else is (for all syms in ns)"
    (let [target-ns 'orchard.info
          ns-syms (keys (ns-map target-ns))
          info-specials (fn [info-params]
                          (as-> (map info/info* info-params) it
                            (group-by :special-form it)
                            (get it true)
                            (map :name it)
                            (set it)))]
      (when cljs-available?
        (testing "- :cljs"
          (let [special-doc-map (misc/require-and-resolve 'cljs.repl/special-doc-map)
                special-syms (into '#{in-ns load load-file} (keys special-doc-map))]
            (is (= special-syms (->> (into special-syms ns-syms)
                                     (map #(merge *cljs-params* {:ns target-ns :sym %}))
                                     (info-specials)))))))
      (testing "- :clj"
        (let [special-doc-map (misc/require-and-resolve 'clojure.repl/special-doc-map)
              special-syms (into '#{letfn let loop fn} (keys special-doc-map))]
          (is (= special-syms (->> (into special-syms ns-syms)
                                   (map #(hash-map :ns target-ns :sym %))
                                   (info-specials)))))))))

(deftest info-var-alias-test
  (testing "Aliased var"
    (let [params '{:ns orchard.test-ns
                   :sym test-dep/foo-in-dep}
          expected '{:ns orchard.test-ns-dep
                     :name foo-in-dep
                     :arglists ([foo])}]
      (when cljs-available?
        (testing "- :cljs"
          (let [i (info/info* (merge *cljs-params* params))]
            (is (= expected (select-keys i [:ns :name :arglists])))
            (is (str/includes? (:file i) "test_ns_dep")))))
      (testing "- :clj"
        (let [i (info/info* params)]
          (is (= expected (select-keys i [:ns :name :arglists])))
          (is (str/includes? (:file i) "test_ns_dep")))))))

(deftest info-resolve-var-before-alias-test
  (testing "resolve a fully qualified var before an alias - test for bug #53"
    (let [expected '{:ns clojure.string
                     :name replace
                     :arglists ([s match replacement])}]
      (when cljs-available?
        (testing "- :cljs"
          (let [i (info/info 'orchard.test-ns 'clojure.string/replace *cljs-params*)]
            (is (= expected (select-keys i [:ns :name :arglists]))))))

      (testing "- :clj"
        (let [i (info/info 'orchard.test-ns 'clojure.string/replace)]
          (is (= expected (select-keys i [:ns :name :arglists])))))))

  (testing "resolve an unqualified alias instead of the var - test for bug #53"
    (let [expected '{:ns clojure.string
                     :name replace
                     :arglists ([s match replacement])}]
      (when cljs-available?
        (testing "- :cljs"
          (let [i (info/info 'orchard.test-ns 'replace *cljs-params*)]
            (is (= expected (select-keys i [:ns :name :arglists]))))))

      (testing "- :clj"
        (let [i (info/info 'orchard.test-ns 'replace)]
          (is (= expected (select-keys i [:ns :name :arglists]))))))))

(deftest info-fully-qualified-var-test
  (testing "Fully-qualified var"
    (let [params '{:ns orchard.test-ns
                   :sym clojure.string/trim}
          expected '{:ns clojure.string
                     :name trim
                     :arglists ([s])
                     :doc "Removes whitespace from both ends of string."}]
      (when cljs-available?
        (testing "- :cljs"
          (is (= expected (-> (info/info* (merge *cljs-params* params))
                              (select-keys [:ns :name :arglists :doc]))))))
      (testing "- :clj"
        (is (= expected (-> (info/info* params)
                            (select-keys [:ns :name :arglists :doc])
                            (update :ns ns-name))))))))

(deftest info-unqualified-sym-and-namespace-test
  (testing "Resolution from current namespace"
    (when cljs-available?
      (testing "- :cljs"
        (let [i (info/info* (merge *cljs-params* '{:ns cljs.core :sym +}))]
          (is (= '+ (:name i)))
          (is (= 'cljs.core (:ns i))))))
    (testing "- :clj"
      (let [i (info/info* '{:ns clojure.core :sym +})]
        (is (= '+ (:name i)))
        (is (= 'clojure.core (:ns i))))))

  (testing "Resolution from other namespaces"
    (when cljs-available?
      (testing "- :cljs"
        (let [i (info/info* (merge *cljs-params* '{:ns cljs.user :sym +}))]
          (is (= (-> #'+ meta :name) (:name i)))
          (is (= 'cljs.core (:ns i))))))
    (testing "- :clj"
      (let [i (info/info* '{:ns user :sym +})]
        (is (= '+ (:name i)))
        (is (= 'clojure.core (:ns i)))))))

(when cljs-available?
  (deftest info-cljs-tooling-issue-28-test
    (testing "Resolution from current namespace - issue #28 from cljs-tooling"
      (let [i (info/info* (merge *cljs-params* '{:ns orchard.test-ns :sym issue-28}))]
        (is (= '{:arglists ([])
                 :line 15
                 :column 1
                 :ns orchard.test-ns
                 :name issue-28}
               (select-keys i [:arglists :line :column :ns :name])))
        (is (str/includes? (:file i) "orchard/test_ns"))))))

(deftest info-ns-as-sym-test
  (testing "Only namespace as qualified symbol"
    (let [params   '{:sym orchard.test-ns}
          expected '{:ns orchard.test-ns
                     :name orchard.test-ns
                     :doc "A test namespace"
                     :line 1}]
      (when cljs-available?
        (testing "- :cljs"
          (let [i (info/info* (merge *cljs-params* params))]
            (is (= expected (select-keys i [:line :doc :name :ns])))
            (is (str/includes? (:file i) "orchard/test_ns")))))
      (testing "- :clj"
        (let [i (info/info* params)]
          (is (= expected (select-keys i [:line :doc :name :ns])))
          (is (str/includes? (:file i) "orchard/test_ns"))))

      (when cljs-available?
        ;; is how the info middleware sends it
        (testing "- :cljs with context"
          (let [params '{:context-ns orchard.test-ns
                         :sym orchard.test-ns}
                i (info/info* (merge *cljs-params* params))]
            (is (= '{:ns orchard.test-ns
                     :name orchard.test-ns
                     :line 1}
                   (select-keys i [:line :name :ns])))
            (is (str/includes? (:file i) "orchard/test_ns"))))))))

(deftest info-ns-dependency-as-sym-test
  (testing "Namespace dependency"
    (let [params '{:sym orchard.test-ns-dep}
          expected '{:ns orchard.test-ns-dep
                     :name orchard.test-ns-dep
                     :doc "Dependency of test-ns namespace"
                     :line 1}]
      (when cljs-available?
        (testing "- :cljs"
          (let [i (info/info* (merge *cljs-params* params))]
            (is (= expected (select-keys i [:line :doc :name :ns])))
            (is (str/includes? (:file i) "orchard/test_ns_dep")))))
      (testing "- :clj"
        (let [i (info/info* params)]
          (is (= expected (select-keys i [:line :doc :name :ns])))
          (is (str/includes? (:file i) "orchard/test_ns_dep"))))

      ;; is how the info middleware sends it
      (when cljs-available?
        (testing "- :cljs with context"
          (let [params '{:sym orchard.test-ns-dep
                         :context-ns orchard.test-ns}
                i (info/info* (merge *cljs-params* params))]
            (is (= '{:ns orchard.test-ns-dep
                     :name orchard.test-ns-dep
                     :doc "Dependency of test-ns namespace"
                     :line 1}
                   (select-keys i [:line :name :doc :ns])))
            (is (str/includes? (:file i) "orchard/test_ns_dep"))))))))

(deftest info-cljs-core-namespace-test
  (testing "Namespace itself but cljs.core"
    (when cljs-available?
      (testing "- :cljs"
        (is (= 'cljs.core (:ns (info/info* (merge *cljs-params* '{:sym cljs.core})))))))
    (testing "- :clj"
      (is (= 'clojure.core (:ns (info/info* '{:sym clojure.core})))))))

(deftest info-namespace-alias-test
  (testing "Namespace alias"
    (let [params '{:ns orchard.test-ns
                   :sym test-dep}
          expected '{:ns orchard.test-ns-dep
                     :name orchard.test-ns-dep
                     :doc "Dependency of test-ns namespace"
                     :line 1}]
      (when cljs-available?
        (testing "- :cljs"
          (let [i (info/info* (merge *cljs-params* params))]
            (is (= expected (select-keys i [:ns :name :doc :arglists :line])))
            (is (str/includes? (:file i) "orchard/test_ns_dep")))))

      (testing "- :clj"
        (let [i (info/info* params)]
          (is (= expected (select-keys i [:ns :name :doc :arglists :line])))
          (is (str/includes? (:file i) "orchard/test_ns_dep")))))))

(deftest info-namespace-macro-test
  (testing "Macro namespace"
    (when cljs-available?
      (testing "- :cljs"
        (let [params '[{:sym orchard.test-macros}
                       {:sym orchard.test-macros
                        :ns orchard.test-ns}
                       {:sym orchard.test-macros
                        :context-ns orchard.test-ns}
                       {:sym orchard.test-macros
                        :context-ns orchard.test-ns}]
              expected '{:ns orchard.test-macros
                         :file "orchard/test_macros.clj"
                         :name orchard.test-macros
                         :line 1}]
          (is (= (take 4 (repeat expected))
                 (map #(info/info* (merge *cljs-params* %)) params))))))))

(deftest info-namespace-cljs-core-macro-test
  (when cljs-available?
    (testing "cljs.core macro"
      (testing "- :cljs"
        (let [params '[{:sym loop}
                       {:sym loop :ns cljs.core}
                       {:sym loop :context-ns cljs.core}
                       {:sym cljs.core/loop}
                       {:sym cljs.core/loop :context-ns cljs.user}
                       {:sym cljs.core/loop :ns cljs.user}]
              expected '{:ns cljs.core
                         :doc "Evaluates the exprs in a lexical context in which the symbols in\n  the binding-forms are bound to their respective init-exprs or parts\n  therein. Acts as a recur target."
                         :name loop
                         :arglists ([bindings & body])}]
          (is (= (take 6 (repeat expected))
                 (->> params
                      (map #(info/info* (merge *cljs-params* %)))
                      (map #(select-keys % [:ns :name :doc :arglists]))))))))))

(deftest info-namespace-macro-alias-test
  (when cljs-available?
    (testing "Macro namespace alias"
      (testing "- :cljs"
        (let [params '[{:sym test-macros :context-ns orchard.test-ns}
                       {:sym test-macros :ns orchard.test-ns}]
              expected '{:ns orchard.test-macros
                         :name orchard.test-macros
                         :file "orchard/test_macros.clj"
                         :line 1}]
          (is (= (take 2 (repeat expected))
                 (map #(info/info* (merge *cljs-params* %)) params))))))))

(deftest info-macros-var-test
  (testing "Macro"
    (when cljs-available?
      (testing "- :cljs"
        (let [params '[{:sym orchard.test-macros/my-add}
                       {:ns orchard.test-macros
                        :sym my-add}]
              expected '{:ns orchard.test-macros
                         :name my-add
                         :arglists ([a b])
                         :macro true
                         :file "orchard/test_macros.clj"}]
          (is (= (take 2 (repeat expected))
                 (->> params
                      (map #(info/info* (merge *cljs-params* %)))
                      (map #(select-keys % [:ns :name :arglists :macro :file]))))))))

    (testing "- :clj"
      (let [params '[{:sym orchard.test-macros/my-add}
                     {:ns orchard.test-macros
                      :sym my-add}]
            expected '{:ns orchard.test-macros
                       :name my-add
                       :arglists ([a b])
                       :macro true
                       :file "orchard/test_macros.clj"}]
        (is (= (take 2 (repeat expected))
               (->> params
                    (map #(info/info* %))
                    (map #(select-keys % [:ns :name :arglists :macro :file])))))))))

(deftest info-macros-referred-var-test
  (testing "Macro - referred"
    (let [params '[{:sym orchard.test-ns/my-add},

                   {:ns orchard.test-ns
                    :sym my-add},

                   {:ns orchard.test-ns
                    :sym orchard.test-ns/my-add}]
          expected '{:name my-add
                     :ns orchard.test-macros
                     :arglists ([a b])
                     :file "orchard/test_macros.clj"
                     :macro true}]

      (when cljs-available?
        (testing "- :cljs"
          (is (= (take 3 (repeat expected))
                 (->> params
                      (map #(info/info* (merge *cljs-params* %)))
                      (map #(select-keys % [:ns :name :arglists :macro :file])))))))

      (testing "- :clj"
        (is (= [{}, expected, {}]
               (->> params
                    (map #(info/info* %))
                    (map #(select-keys % [:ns :name :arglists :macro :file])))))))))

(deftest info-macros-scoped-var-test
  (testing "Macro - scoped"
    (let [params '[{:ns orchard.test-ns
                    :sym test-macros/my-add}]
          expected '{:name my-add
                     :ns orchard.test-macros
                     :arglists ([a b])
                     :file "orchard/test_macros.clj"
                     :macro true}]

      (when cljs-available?
        (testing "- :cljs"
          (is (= (take 1 (repeat expected))
                 (->> params
                      (map #(info/info* (merge *cljs-params* %)))
                      (map #(select-keys % [:ns :name :arglists :macro :file])))))))

      (testing "- :clj"
        (is (= (take 1 (repeat expected))
               (->> params
                    (map #(info/info* %))
                    (map #(select-keys % [:ns :name :arglists :macro :file])))))))))

(deftest info-no-file-info-test
  (testing "File info key does not exist should not resolve classpath - issue #61"
    (let [params '{:sym finally}
          expected '{:forms [(try expr* catch-clause* finally-clause?)],
                     :doc "catch-clause => (catch classname name expr*)\n  finally-clause => (finally expr*)\n\n  Catches and handles Java exceptions.",
                     :name finally,
                     :special-form true,
                     :url "https://clojure.org/special_forms#finally"}]
      (testing "- boot project"
        (with-redefs [orchard.misc/boot-project? (constantly true)]
          (let [i (info/info* params)]
            (is (= expected (select-keys i [:ns :name :doc :forms :special-form :url])))
            (is (nil? (:file i))))))

      (testing "- no boot project"
        (let [i (info/info* params)]
          (is (= expected (select-keys i [:ns :name :doc :forms :special-form :url])))
          (is (nil? (:file i))))))))

(deftest file-resolution-no-defs-issue-75-test
  (testing "File resolves, issue #75"
    (let [params '{:ns orchard.test-ns
                   :sym orchard.test-no-defs}
          f "orchard/test_no_defs.cljc"]
      (when cljs-available?
        (testing "- :cljs"
          (let [cljs-merged-params (merge *cljs-params* params)]
            (is (-> cljs-merged-params info/info* ^String (:file) (.endsWith f))))))

      (testing "- :clj"
        (is (-> params info/info* ^String (:file) (.endsWith f)))))))

;;;;;;;;;;;;;;;;;;
;; Clojure Only ;;
;;;;;;;;;;;;;;;;;;

(deftest see-also-test
  (let [expected '(clojure.core/map-indexed
                   clojure.core/pmap
                   clojure.core/amap
                   clojure.core/mapcat
                   clojure.core/keep
                   clojure.core/juxt
                   clojure.core/mapv
                   clojure.core/reduce
                   clojure.core/run!)]

    (testing "info/see-also through info/info* in a required namespace"
      (is (= expected (-> '{:ns orchard.test-ns :sym map}
                          info/info*
                          :see-also))))))

(deftest info-jvm-test
  (is (info/info* {:ns 'orchard.info :sym 'java.lang.Class}))
  (is (info/info* {:ns 'orchard.info :sym 'Class/forName}))
  (is (info/info* {:ns 'orchard.info :sym '.toString})))

(deftest info-java-test
  (is (info/info-java 'clojure.lang.Atom 'swap)))

(when util/has-enriched-classpath?
  (deftest info-java-member-precedence-test
    (testing "Integer/max - issue #86"
      (let [i (info/info* {:ns 'user :sym 'Integer/max})]
        (is (= (select-keys i [:class :member :modifiers :throws :argtypes :arglists :returns])
               '{:throws ()
                 :argtypes [int int]
                 :member max
                 :modifiers #{:public :static}
                 :class java.lang.Integer
                 :arglists ([a b])
                 :returns int}))
        (is (re-find #"Returns the greater of two" (:doc i)))))))

(def some-var nil)

(def workaround
  "Prevents a clj-kondo warning."
  replace-first)

(deftest info-undefined-namespace-test
  (let [current-ns (-> ::_ namespace symbol)]
    (are [input expected] (= expected
                             (select-keys (info/info* input)
                                          [:added :ns :name :file]))
      {:ns current-ns :sym 'does-not-exist}             {}
      {:ns current-ns :sym 'some-var}                   '{:ns   orchard.info-test,
                                                          :name some-var,
                                                          :file "orchard/info_test.clj"}
      {:ns current-ns :sym 'replace-first}              '{:added "1.2",
                                                          :ns    clojure.string,
                                                          :name  replace-first,
                                                          :file  "clojure/string.clj"}
      {:ns current-ns :sym 'merge}                      '{:added "1.0"
                                                          :ns    clojure.core
                                                          :name  merge
                                                          :file  "clojure/core.clj"}
      {:ns current-ns :sym 'non.existing.ns/merge}      {}
      {:ns current-ns :sym 'clojure.string/upper-case}  '{:added "1.2"
                                                          :ns    clojure.string
                                                          :name  upper-case
                                                          :file  "clojure/string.clj"}
      {:ns current-ns :sym 'non.existing.ns/upper-case} {}

      {:ns 'gibberish :sym 'does-not-exist}             {}
      {:ns 'gibberish :sym 'some-var}                   {}
      {:ns 'gibberish :sym 'replace-first}              {}
      {:ns 'gibberish :sym 'merge}                      '{:added "1.0"
                                                          :ns    clojure.core
                                                          :name  merge
                                                          :file  "clojure/core.clj"}
      {:ns 'gibberish :sym 'non.existing.ns/merge}      {}
      {:ns 'gibberish :sym 'clojure.string/upper-case}  '{:added "1.2"
                                                          :ns    clojure.string
                                                          :name  upper-case
                                                          :file  "clojure/string.clj"}
      {:ns 'gibberish :sym 'non.existing.ns/upper-case} {})))

(deftest javadoc-info-unit-test
  (testing "Get an HTTP URL for a Sun/Oracle Javadoc"
    (testing "Javadoc 1.7 format"
      (let [reply      (info/javadoc-info "java/lang/StringBuilder.html#capacity()")
            url        (:javadoc reply)
            exp-suffix "/docs/api/java/lang/StringBuilder.html#capacity()"]
        (is (str/includes? url exp-suffix))))

    (testing "Javadoc 1.8 format"
      (let [reply      (info/javadoc-info "java/lang/StringBuilder.html#capacity--")
            url        (:javadoc reply)
            exp-suffix "/docs/api/java/lang/StringBuilder.html#capacity--"]
        (is (str/includes? url exp-suffix)))))

  (testing "Get general URL for a clojure javadoc"
    (let [reply    (info/javadoc-info "clojure/java/io.clj")
          url      (:javadoc reply)
          url-type (class url)
          exp-type java.net.URL]
      (is (= url-type exp-type))))

  (testing "Get URL for commonly used Java libraries via the *remote-javadocs* mechanism"
    (let [reply    (info/javadoc-info "com/amazonaws/services/lambda/AWSLambdaClient.html#listFunctions()")
          url      (:javadoc reply)]
      (is (= url "http://docs.aws.amazon.com/AWSJavaSDK/latest/javadoc/com/amazonaws/services/lambda/AWSLambdaClient.html#listFunctions()"))))

  (testing "Get fall through URL type for other Javadocs (external libs?)"
    (let [reply (info/javadoc-info "http://some/other/url")
          url (:javadoc reply)]
      (is (= url "http://some/other/url")))))

;; TODO: Assess the value of this test
(deftest javadoc-url-test
  (when (= misc/java-api-version 7)
    (testing "java 1.7"
      (is (= "java/lang/StringBuilder.html#charAt(int)"
             (-> (info/info-java 'java.lang.StringBuilder 'charAt)
                 (get :javadoc))))))

  (when (= misc/java-api-version 8)
    (testing "java 1.8"
      (is (= "java/lang/StringBuilder.html#charAt-int-"
             (-> (info/info-java 'java.lang.StringBuilder 'charAt)
                 (get :javadoc))))))

  (when (= misc/java-api-version 9)
    (testing "java 9"
      (is (= "java/lang/StringBuilder.html#charAt-int-"
             (-> (info/info-java 'java.lang.StringBuilder 'charAt)
                 (get :javadoc)))))))

;;; resource path test
(defn file
  [x]
  (:file (info/file-info x)))

(defn relative
  [x]
  (:resource (info/file-info x)))

(deftest resource-path-test
  (is (= (class (file (subs (str (io/resource "clojure/core.clj")) 4)))
         java.net.URL))
  (is (= (class (file "clojure/core.clj"))
         java.net.URL))
  (is (= (class (file "clojure-1.7.0.jar:clojure/core.clj"))
         java.net.URL))
  (is (= (class (file "orchard/test_ns.cljc"))
         java.net.URL))
  (is (relative "clojure/core.clj"))
  (is (nil? (relative "notclojure/core.clj"))))

(deftest qualify-sym-test
  (is (= '+ (info/qualify-sym nil '+)))
  (is (nil? (info/qualify-sym 'cljs.core nil)))
  (is (nil? (info/qualify-sym  nil nil)))
  (is (= 'cljs.core/+ (info/qualify-sym 'cljs.core '+))))

(deftest normalize-params-test
  (testing ":qualified-sym namespace coming from :ns"
    (is (= 'cljs.core/+ (-> '{:ns cljs.core
                              :sym +
                              :context-ns orchard.info}
                            info/normalize-params
                            :qualified-sym))))

  (testing ":qualified-sym namespace coming from :context-ns if :ns is missing"
    (is (= 'orchard.info/+ (-> '{:sym + :context-ns orchard.info}
                               info/normalize-params
                               :qualified-sym))))

  (testing "adding :qualified-sym if :sym is qualified"
    (is (= '{:sym orchard.info/+
             :qualified-sym orchard.info/+}
           (-> '{:sym orchard.info/+}
               (info/normalize-params)
               (select-keys [:sym :qualified-sym])))))

  (testing "adding :computed-ns if :sym is qualified"
    (is (= '{:sym orchard.info/+
             :computed-ns orchard.info}
           (-> '{:sym orchard.info/+}
               (info/normalize-params)
               (select-keys [:sym :computed-ns])))))

  (testing "adding :unqualified-sym if :sym is qualified"
    (is (= '{:sym orchard.info/+
             :unqualified-sym +}
           (-> '{:sym orchard.info/+}
               (info/normalize-params)
               (select-keys [:sym :unqualified-sym])))))

  (testing "adding :unqualified-sym if :sym is unqualified"
    (is (= '{:sym +
             :unqualified-sym +}
           (-> '{:sym +}
               (info/normalize-params)
               (select-keys [:sym :unqualified-sym])))))

  (testing "in case of :ns only it should always assoc :unqualified-sym"
    (is (= '{:ns orchard.info
             :unqualified-sym orchard.info}
           (-> '{:ns orchard.info}
               (info/normalize-params)
               (select-keys [:ns :unqualified-sym]))))))

(deftest boot-file-resolution-test
  ;; this checks the files on the classpath soo you need the test-resources
  ;; and specifically test-resources/orchard/test_ns.cljc
  ;;
  ;; Note that :file in :meta is left untouched
  (when cljs-available?
    (with-redefs [orchard.misc/boot-project? (constantly true)]
      (is (= '{:ns orchard.test-ns
               :name x
               :file "orchard/test_ns.cljc"}
             (-> (merge *cljs-params* '{:ns orchard.test-ns :sym x})
                 (info/info*)
                 (select-keys [:ns :name :file])))))))
