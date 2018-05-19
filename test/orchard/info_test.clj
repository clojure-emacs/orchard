(ns orchard.info-test
  (:require [clojure.test :refer :all]
            [clojure.repl :as repl]
            [orchard.info :as info]
            [orchard.misc :as misc]))

(deftest see-also-test
  (is (not-empty (info/see-also 'clojure.core 'map))))

(deftype T [])

(deftest info-test
  (is (info/info 'orchard.info 'io))

  (is (info/info 'orchard.info 'info))

  (is (info/info 'orchard.info 'java.lang.Class))
  (is (info/info 'orchard.info 'Class/forName))
  (is (info/info 'orchard.info '.toString))

  (is (not (info/info 'clojure.core (gensym "non-existing"))))
  (is (info/info 'orchard.info-test 'T)
      "Check that deftype T (which returns nil for .getPackage), doesn't throw")

  (is (= (the-ns 'clojure.core) (:ns (info/info 'orchard.info 'str))))

  ;; special forms are marked as such and nothing else is (for all syms in ns)
  (let [ns 'orchard.info
        spec-forms (into '#{letfn fn let loop} (keys @#'repl/special-doc-map))
        infos (->> (into spec-forms (keys (ns-map ns)))
                   (map (partial info/info ns)))]
    (is (= spec-forms (->> (-> (group-by :special-form infos)
                               (get true))
                           (map :name)
                           (set))))))

(deftest info-java-test
  (is (info/info-java 'clojure.lang.Atom 'swap)))

(deftest javadoc-info-unit-test
  (testing "Get an HTTP URL for a Sun/Oracle Javadoc"
    (testing "Javadoc 1.7 format"
      (let [reply      (info/javadoc-info "java/lang/StringBuilder.html#capacity()")
            url        (:javadoc reply)
            exp-suffix "/docs/api/java/lang/StringBuilder.html#capacity()"]
        (is (.endsWith url exp-suffix))))

    (testing "Javadoc 1.8 format"
      (let [reply      (info/javadoc-info "java/lang/StringBuilder.html#capacity--")
            url        (:javadoc reply)
            exp-suffix "/docs/api/java/lang/StringBuilder.html#capacity--"]
        (is (.endsWith url exp-suffix)))))

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
  (if (= misc/java-api-version 7)
    (testing "java 1.7"
      (is (= "java/lang/StringBuilder.html#charAt(int)"
             (-> (info/info-java 'java.lang.StringBuilder 'charAt)
                 (get :javadoc))))))

  (if (= misc/java-api-version 8)
    (testing "java 1.8"
      (is (= "java/lang/StringBuilder.html#charAt-int-"
             (-> (info/info-java 'java.lang.StringBuilder 'charAt)
                 (get :javadoc))))))

  (if (= misc/java-api-version 9)
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
  (is (= (class (file (subs (str (clojure.java.io/resource "clojure/core.clj")) 4)))
         java.net.URL))
  (is (= (class (file "clojure/core.clj"))
         java.net.URL))
  (is (= (class (file "clojure-1.7.0.jar:clojure/core.clj"))
         java.net.URL))
  (is (= (class (file "test/orchard/info_test.clj"))
         java.net.URL))
  (is (relative "clojure/core.clj"))
  (is (nil? (relative "notclojure/core.clj")))
  (is (nil? (info/resource-path "jar:file:fake.jar!/fake/file.clj"))))
