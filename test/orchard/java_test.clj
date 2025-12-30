(ns orchard.java-test
  (:require
   [clojure.java.javadoc :as javadoc]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer [are deftest is testing]]
   [matcher-combinators.matchers :as mc]
   [orchard.java :as sut :refer [cache class-info class-info* javadoc-url member-info resolve-class resolve-javadoc-path resolve-member resolve-symbol source-info]]
   [orchard.misc :as misc]
   [orchard.test.util :as util :refer [is+]])
  (:import
   (mx.cider.orchard LruMap)))

(def ^:private jdk11+? (>= misc/java-api-version 11))

(defn- *ns [] 'orchard.java-test)

(javadoc/add-remote-javadoc "com.amazonaws." "http://docs.aws.amazon.com/AWSJavaSDK/latest/javadoc/")
(javadoc/add-remote-javadoc "org.apache.kafka." "https://kafka.apache.org/090/javadoc/")

(when (and jdk11+? util/jdk-sources-present?)
  (deftest source-info-test
    (testing "Parse tree kinds"
      (testing "for non-existent classes"
        (is (not (source-info 'not.actually.AClass))))
      (testing "for Clojure classes"
        (is (-> (source-info 'clojure.lang.ISeq) :line)) ; interface
        (is (-> (source-info 'clojure.lang.AFn) :line))  ; abstract class
        (is (-> (source-info 'clojure.lang.APersistentMap$ValSeq) :line)) ; nested class
        (is (-> (source-info 'clojure.lang.Numbers$Ops) :line)) ; nested default interface
        (is (-> (source-info 'clojure.lang.Range$BoundsCheck) :line)) ; nested private interface
        (is (-> (source-info 'clojure.lang.Numbers$Category) :line)) ; nested enum
        (is (not (source-info 'clojure.core.Eduction)))) ; record
      (testing "for JDK classes"
        (is (-> (source-info 'java.util.Collection) :line)) ; interface
        (is (-> (source-info 'java.util.AbstractCollection) :line)) ; abstract class
        (is (-> (source-info 'java.lang.Thread$UncaughtExceptionHandler) :line)) ; nested interface
        (is (-> (source-info 'java.net.Authenticator$RequestorType) :line)))) ; nested enum

    (testing "Source parsing"
      (testing "for Clojure classes"
        (is (-> (source-info 'clojure.lang.ExceptionInfo) :doc))
        (is+ {:members {'multiply {['clojure.lang.BigInt] {:line number?}}}}
             (source-info 'clojure.lang.BigInt)))
      (testing "for JDK classes"
        (is+ {:doc string?
              :members {'size {[] {:line number?}}}}
             (source-info 'java.util.AbstractCollection))))))

(defn class-corpus []
  {:post [(> (count %)
             50)]}
  (->> (util/imported-classes 'clojure.core)
       (into ['java.util.Map 'java.io.File 'clojure.lang.Compiler])
       (into (util/imported-classes (-> ::_ namespace symbol)))
       ;; Remove classes without methods:
       (remove #(or (#{`ThreadDeath
                       `Void
                       `RuntimePermission
                       'clojure.core.Vec
                       'clojure.core.VecNode
                       'clojure.core.VecSeq
                       'clojure.core.ArrayChunk
                       'clojure.core.Eduction
                       ;; TODO: figure out why it started failing for BigInteger
                       'java.math.BigInteger
                       ;; Currently doesn't work for LruMap.
                       'mx.cider.orchard.LruMap} %)
                    (-> % str Class/forName .isInterface)
                    (-> % str Class/forName .isEnum)
                    (re-find #"(Exception|Error)$" (str %))))
       vec))

(defn extract-method-arities [class-symbol info]
  {:pre [(symbol? class-symbol)]}
  (->> (-> info
           :members
           vals)
       (mapcat vals)
       ;; Only methods/constructors (and not fields) have arglists:
       (filter (fn [{:keys [returns] n :name}]
                 (or returns
                     (= n class-symbol))))))

(when (and jdk11+? util/jdk-sources-present?)
  (deftest map-structure-test
    (testing "Parsed map structure = reflected map structure"
      (doseq [class-sym (class-corpus)]
        (testing class-sym
          (let [excluded-cols #{:file :line :column :doc :argnames :non-generic-argtypes :annotated-arglists
                                :doc-first-sentence-fragments :doc-fragments :doc-block-tags-fragments :argtypes :path :resource-url}
                extract-keys (fn [x]
                               (->> excluded-cols
                                    (apply dissoc x)
                                    (keys)
                                    (set)
                                    (sort-by pr-str)))
                assert-keys= (fn [a b]
                               (let [aa (extract-keys a)
                                     bb (extract-keys b)]
                                 (testing (pr-str {:only-in-reflector (remove (set aa) bb)
                                                   :only-in-full (remove (set bb) aa)})
                                   (is (= aa bb)))))
                full-class-info (class-info* class-sym)
                reflector-class-info (with-redefs [source-info (constantly nil)]
                                       (class-info* class-sym))
                arities (extract-method-arities class-sym full-class-info)
                all-annotated-arglists (->> arities
                                            (map (fn [{:keys [annotated-arglists]
                                                       n :name}]
                                                   [annotated-arglists
                                                    (= n class-sym)])))]
            (testing "Class info"
              (assert-keys= full-class-info reflector-class-info))

            (let [full-class-info-members (:members full-class-info)
                  reflector-class-info-members (:members reflector-class-info)]
              (testing "Members info"
                (is (seq full-class-info-members))
                (is (empty? (set/difference (set (keys reflector-class-info-members))
                                            (set (keys full-class-info-members))))
                    {:reflector-arities (keys reflector-class-info-members)
                     :only-in-full (keys full-class-info-members)}))
              (testing "Arities info"
                (doseq [k (keys full-class-info-members)]
                  (testing (str "arity " k)
                    (let [reflector-arities (set (keys (reflector-class-info-members k)))
                          full-arities (set (keys (full-class-info-members k)))]
                      (is (empty? (set/difference reflector-arities full-arities))
                          {:reflector-arities reflector-arities
                           :only-in-full full-arities}))))))

            (is (pos? (count all-annotated-arglists)))
            (doseq [[s constructor?] all-annotated-arglists]
              (is (string? s))
              (testing s
                (if constructor?
                  (is (re-find #"^\[" s))
                  (is (re-find #"\^.*\[" s)))
                ;; Assert that the format doesn't include past bugs:
                (is (not (str/includes? s "<")))
                (is (not (str/includes? s "^Object java.lang.Object")))
                (is (not (str/includes? s "^Object Object")))
                (is (not (str/includes? s "^function.Function java.util.function.Function")))
                (is (not (str/includes? s "^java.util.function.Function java.util.function.Function")))
                (is (not (str/includes? s "java.lang")))))))))))

(when (and jdk11+? util/jdk-sources-present?)
  (deftest class-info-test
    (let [c1 (class-info 'clojure.lang.Agent)
          c2 (class-info 'clojure.lang.Range$BoundsCheck)
          c3 (class-info 'not.actually.AClass)
          thread-class-info (class-info `Thread)]
      (testing "Class"
        (is+ {:file misc/url?} c1)
        (is+ {:file misc/url?} c2)
        (is+ nil c3 "that doesn't exist")
        (testing "member info"
          (when (is+ (mc/seq-of map?) (vals (:members c1)))
            (is+ (mc/all-of
                  not-empty
                  (mc/seq-of {:name some?
                              :modifiers some?
                              :annotated-arglists string?}))
                 (mapcat vals (vals (:members c1))))))
        (testing "doesn't throw on classes without dots in classname"
          (let [reified (binding [*ns* (create-ns 'foo)]
                          (clojure.core/eval
                           '(clojure.core/reify Object)))
                sym (symbol (.getName (class reified)))]
            (is (class-info sym)))))
      (testing "Doc fragments"
        (is+ {:doc-fragments seq
              :doc-first-sentence-fragments seq}
             thread-class-info)))))

(when (and jdk11+? util/jdk-sources-present?)
  (deftest member-info-test
    (let [m1 (member-info 'clojure.lang.PersistentHashMap 'assoc)
          m2 (member-info 'java.util.AbstractCollection 'non-existent-member)
          m3 (member-info 'not.actually.AClass 'nada)
          m4 (member-info 'java.awt.Point 'x)
          m5 (member-info 'java.lang.Class 'forName)
          m6 (member-info 'java.util.AbstractMap 'finalize)
          m7 (member-info 'java.util.HashMap 'finalize)
          m8 (member-info `Thread 'isDaemon)]
      (testing "Member"
        (is+ {:file misc/url?
              :line number?
              :arglists (mc/all-of vector? (mc/seq-of vector?))
              :annotated-arglists (mc/all-of vector? (mc/seq-of string?))}
             m1)
        (is+ nil m2 "that doesn't exist")
        (is+ nil m3 "in a class that doesn't exist")
        (is m4 "that is a field")
        (is m5 "that is static")
        (testing "implemented on immediate superclass"
          (is (not= 'java.lang.Object (:class m6)))
          (is (not= 'java.lang.Object (:class m7)))
          (is+ {:doc #"^Called by the garbage collector on an object when garbage collection"}
               m6
               "Contains doc that is clearly defined in Object (the superclass)"))
        (testing "Doc fragments"
          (testing "For a field"
            (is+ {:doc-fragments seq
                  :doc-first-sentence-fragments seq}
                 m4))

          (testing "For a method"
            (is+ {:doc-fragments seq
                  :doc-first-sentence-fragments seq}
                 m8)))))))

(deftest arglists-test
  (let [+this (comp #{'this} first)]
    (testing "Arglist prepending of 'this'"
      (testing "for instance methods"
        (is (every? +this (:arglists (member-info 'java.lang.StringWriter 'write)))))
      (testing "for instance fields"
        (is (every? +this (:arglists (member-info 'java.awt.Point 'x)))))
      (testing "for static members"
        (is (not-any? +this (:arglists (member-info 'java.lang.Class 'forName)))))
      (testing "for constructors"
        (is (not-any? +this (:arglists (member-info 'java.lang.String
                                                    'java.lang.String))))))))

(deftest javadoc-urls-test
  (testing "Javadoc URL"
    (when-not jdk11+?
      (testing "for Java 8"
        (with-redefs [cache (LruMap. 100)]
          (testing "of a class"
            (is (= "java/lang/String.html"
                   (:javadoc (class-info 'java.lang.String)))))

          (testing "of a nested class"
            (is (= "java/util/AbstractMap.SimpleEntry.html"
                   (:javadoc (class-info 'java.util.AbstractMap$SimpleEntry)))))

          (testing "of an interface"
            (is (= "java/io/Closeable.html"
                   (:javadoc (class-info 'java.io.Closeable)))))

          (testing "of a class member"
            (testing "with no args"
              (is (= "java/util/Random.html#nextLong--"
                     (:javadoc (member-info 'java.util.Random 'nextLong)))))
            (testing "with primitive args"
              (is (= "java/util/Random.html#setSeed-long-"
                     (:javadoc (member-info 'java.util.Random 'setSeed)))))
            (testing "with object args"
              (is (= "java/lang/String.html#contains-java.lang.CharSequence-"
                     (:javadoc (member-info 'java.lang.String 'contains)))))
            (testing "with array args"
              (is (= "java/lang/Thread.html#enumerate-java.lang.Thread:A-"
                     (:javadoc (member-info 'java.lang.Thread 'enumerate)))))
            (testing "with multiple args"
              (is (= "java/util/ArrayList.html#subList-int-int-"
                     (:javadoc (member-info 'java.util.ArrayList 'subList)))))
            (testing "with generic type erasure"
              (is (= "java/util/Hashtable.html#putAll-java.util.Map-"
                     (:javadoc (member-info 'java.util.Hashtable 'putAll)))))))))

    ;; Java 11+ URLs require module information, which is only available on Java 11+.
    (when jdk11+?
      (testing "for Java 11+"
        (with-redefs [cache (LruMap. 100)]
          (testing "of a class"
            (is (= "java.base/java/lang/String.html"
                   (:javadoc (class-info 'java.lang.String)))))

          (testing "of a nested class"
            (is (= "java.base/java/util/AbstractMap.SimpleEntry.html"
                   (:javadoc (class-info 'java.util.AbstractMap$SimpleEntry)))))

          (testing "of an interface"
            (is (= "java.base/java/io/Closeable.html"
                   (:javadoc (class-info 'java.io.Closeable)))))

          (testing "of a class member"
            (testing "with no args"
              (is (= "java.base/java/util/Random.html#nextLong()"
                     (:javadoc (member-info 'java.util.Random 'nextLong)))))
            (testing "with primitive args"
              (is (= "java.base/java/util/Random.html#setSeed(long)"
                     (:javadoc (member-info 'java.util.Random 'setSeed)))))
            (testing "with object args"
              (is (= "java.base/java/lang/String.html#contains(java.lang.CharSequence)"
                     (:javadoc (member-info 'java.lang.String 'contains)))))
            (testing "with array args"
              (is (= "java.base/java/lang/Thread.html#enumerate(java.lang.Thread[])"
                     (:javadoc (member-info 'java.lang.Thread 'enumerate)))))
            (testing "with multiple args"
              (is (= "java.base/java/util/ArrayList.html#subList(int,int)"
                     (:javadoc (member-info 'java.util.ArrayList 'subList)))))
            (testing "with generic type erasure"
              (is (= "java.base/java/util/Hashtable.html#putAll(java.util.Map)"
                     (:javadoc (member-info 'java.util.Hashtable 'putAll)))))))))))

(deftest resolve-javadoc-path-test
  (let [get-url (comp resolve-javadoc-path (partial apply javadoc-url))]
    (when-not jdk11+?
      (testing "Java 8 javadocs resolve to the correct urls"
        (with-redefs [cache (LruMap. 100)]
          (are [class url] (= url (get-url class))
            ['java.lang.String]
            "https://docs.oracle.com/javase/8/docs/api/java/lang/String.html"

            ['java.lang.String 'contains nil]
            "https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#contains"

            ['java.lang.String 'contains ['java.lang.CharSequence]]
            "https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#contains-java.lang.CharSequence-"))))

    (when jdk11+?
      (testing "Java 11 javadocs resolve to the correct urls"
        (with-redefs [misc/java-api-version 11
                      cache (LruMap. 100)]
          (testing "java.base modules resolve correctly"
            (are [class url] (= url (get-url class))
              ['java.lang.String]
              "https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html"

              ['java.lang.String 'contains nil]
              "https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html#contains"

              ['java.lang.String 'contains ['java.lang.CharSequence]]
              "https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html#contains(java.lang.CharSequence)"))

          (testing "non java.base modules also resolve correctly"
            (are [class url] (= url (get-url class))
              ['java.net.http.HttpClient]
              "https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html"

              ['java.net.http.HttpClient 'newHttpClient nil]
              "https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html#newHttpClient"

              ['java.net.http.HttpRequest 'newBuilder ['java.net.URI]]
              "https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpRequest.html#newBuilder(java.net.URI)")))))

    (testing "Allows for added javadocs"
      (with-redefs [cache (LruMap. 100)]
        (is (= "http://docs.aws.amazon.com/AWSJavaSDK/latest/javadoc/com/amazonaws/services/lambda/AWSLambdaClient.html"
               (get-url ['com.amazonaws.services.lambda.AWSLambdaClient])))
        (is (= "https://kafka.apache.org/090/javadoc/org/apache/kafka/clients/consumer/ConsumerConfig.html"
               (get-url '[org.apache.kafka.clients.consumer.ConsumerConfig])))))))

(deftest class-resolution-test
  (testing "Class resolution"
    (testing "of resolvable classes"
      (is+ 'java.lang.String (:class (resolve-class (*ns) 'String)))
      (is+ 'java.lang.String (:class (resolve-class (*ns) 'java.lang.String))))
    (testing "of non-resolvable 'classes'"
      (is+ nil (resolve-class (*ns) 'NothingHere))
      (is+ nil (resolve-class (*ns) 'not.actually.AClass)))
    (testing "of things that aren't classes"
      (is+ nil (resolve-class (*ns) 'assoc))
      (is+ nil (resolve-class (*ns) 'clojure.core)))))

(deftest member-resolution-test
  (testing "Member resolution"
    (testing "of instance members"
      (is+ (mc/seq-of {:member 'toString})
           (resolve-member (*ns) 'toString)))
    (testing "of non-members"
      (is+ [] (resolve-member (*ns) 'notAMember)))))

(deftest symbol-resolution-test
  (testing "Symbol resolution"
    (testing "of classes"
      (is+ 'java.lang.String (:class (resolve-symbol (*ns) 'String))))
    (testing "of deftype in clojure.core"
      (is+ 'clojure.core.Eduction (:class (resolve-symbol 'clojure.core 'Eduction))))
    (testing "of constructors"
      (is+ 'java.lang.String (:class (resolve-symbol (*ns) 'String.))))
    (testing "of unambiguous instance members"
      (is+ 'java.lang.SecurityManager
           (:class (resolve-symbol (*ns) '.checkPackageDefinition)))
      (is+ nil (:class (resolve-symbol (*ns) '.currentThread))
           "Shouldn't resolve since Thread/currentThread is a static method"))
    (testing "of qualified instance members"
      (is+ 'java.lang.Thread (:class (resolve-symbol (*ns) 'Thread/.start))))
    (testing "of candidate instance members"
      (is+ (mc/all-of not-empty
                      (mc/seq-of {:member 'toString}))
           (vals (:candidates (resolve-symbol (*ns) '.toString)))))
    (testing "of static methods"
      (is+ 'forName (:member (resolve-symbol (*ns) 'Class/forName))))
    (testing "of static fields"
      (is+ 'TYPE (:member (resolve-symbol (*ns) 'Void/TYPE))))
    (testing "of java-style printed members"
      (is (= (resolve-symbol (*ns) 'Thread/.start)
             (resolve-symbol (*ns) 'Thread.start)))
      (is (= (resolve-symbol (*ns) 'Thread/currentThread)
             (resolve-symbol (*ns) 'Thread.currentThread)))
      (is (= (resolve-symbol (*ns) 'clojure.lang.Compiler$DefExpr/.eval)
             (resolve-symbol (*ns) 'clojure.lang.Compiler$DefExpr.eval)))
      (is (= 'clojure.lang.Compiler$DefExpr
             (:class (resolve-symbol (*ns) 'clojure.lang.Compiler$DefExpr.eval)))))
    (testing "of module-prefixed classes"
      (is (= (resolve-symbol (*ns) 'java.lang.Thread)
             (resolve-symbol (*ns) 'java.base/java.lang.Thread))))
    (testing "of java-style printed members with module prefix"
      (is (= (resolve-symbol (*ns) 'java.lang.Thread/.run)
             (resolve-symbol (*ns) 'java.base/java.lang.Thread.run))))

    (testing "equality of qualified vs unqualified"
      (testing "classes"
        (is (= (resolve-symbol (*ns) 'java.lang.String)
               (resolve-symbol (*ns) 'String))))
      (testing "constructors"
        (is (= (resolve-symbol (*ns) 'java.lang.Exception.)
               (resolve-symbol (*ns) 'Exception.))))
      (testing "static methods"
        (is (= (resolve-symbol (*ns) 'java.lang.Class/forName)
               (resolve-symbol (*ns) 'Class/forName))))
      (testing "static fields"
        (is (= (resolve-symbol (*ns) 'java.lang.Void/TYPE)
               (resolve-symbol (*ns) 'Void/TYPE))))
      (testing "qualified members"
        (is (= (resolve-symbol (*ns) 'Thread/.start)
               (resolve-symbol (*ns) 'java.lang.Thread/.start))))
      (testing "java-style printed members"
        (is (= (resolve-symbol (*ns) 'Thread.start)
               (resolve-symbol (*ns) 'java.lang.Thread.start)))
        (is (= (resolve-symbol (*ns) 'Thread.currentThread)
               (resolve-symbol (*ns) 'java.lang.Thread.currentThread)))))

    (when util/jdk-sources-present?
      (testing "class and constructor resolve to different lines"
        (is (not= (:line (resolve-symbol (*ns) 'java.lang.String))
                  (:line (resolve-symbol (*ns) 'java.lang.String.))))
        (is (not= (:line (resolve-symbol (*ns) 'Thread))
                  (:line (resolve-symbol (*ns) 'Thread.))))))

    (testing "of things that shouldn't resolve"
      (is (nil? (resolve-symbol (*ns) 'MissingUnqualifiedClass)))
      (is (nil? (resolve-symbol (*ns) 'missing.qualified.Class)))
      (is (nil? (resolve-symbol (*ns) 'MissingUnqualifiedCtor.)))
      (is (nil? (resolve-symbol (*ns) 'missing.qualified.Ctor.)))
      (is (nil? (resolve-symbol (*ns) 'MissingUnqualified/staticMethod)))
      (is (nil? (resolve-symbol (*ns) 'missing.Qualified/staticMethod)))
      (is (nil? (resolve-symbol (*ns) '.missingDottedMethod)))
      (is (nil? (resolve-symbol (*ns) '.random.bunch/of$junk))))))

(defn- replace-last-dot [^String s]
  (if (str/includes? s ".")
    (str/replace s #"\.([^\.]+)$" "\\$$1")
    s))

(when (and util/jdk-sources-present? jdk11+?)
  (deftest reflect-and-source-info-match
    (testing "reflect and source info structurally match, allowing a meaningful deep-merge of both"
      (let [extract-arities (fn [info]
                              (->> info :members vals (mapcat keys)
                                   (remove nil?) ;; fields
                                   (sort-by pr-str)))
            resolves? #(let [s (str/replace (str %) "[]" "")]
                         (if (#{"byte" "short" "int" "long" "float" "double" "char" "boolean" "void"} s)
                           true
                           (try
                             (Class/forName s)
                             (catch Exception _
                               (Class/forName (replace-last-dot s))))))]
        (doseq [class-symbol (class-corpus)
                :let [src-info (source-info class-symbol)
                      reflect-info (sut/reflect-info (#'sut/reflection-for (resolve class-symbol)))
                      arities-from-source (extract-arities src-info)
                      arities-from-reflector (extract-arities reflect-info)]]
          (testing class-symbol
            (when (is (pos? (count arities-from-source)))
              (is+ arities-from-source arities-from-reflector)
              (doseq [arity arities-from-source
                      s arity]
                (is+ resolves? s))

              (let [arities-data (extract-method-arities class-symbol
                                                         (misc/deep-merge reflect-info src-info))]
                (is (pos? (count arities-data)))
                (is+ (mc/seq-of {:argnames some?}) arities-data)))))))))

(when (and util/jdk-sources-present? jdk11+?)
  (deftest array-arg-doc-test
    (testing "Regression test for #278"
      (is (= "^Path [^String first, ^String[] more]"
             (get-in (sut/class-info* 'java.nio.file.Path)
                     [:members 'of ['java.lang.String (symbol "java.lang.String[]")]
                      :annotated-arglists]))))))

(when (and util/jdk-sources-present? jdk11+?)
  (deftest *analyze-sources*-test
    (with-redefs [cache (LruMap. 100)]
      (binding [sut/*analyze-sources* false]
        (is (nil? (:doc (sut/resolve-symbol 'user `Thread/activeCount)))
            "Binding this var to `false` results in source info being omitted"))
      (is (seq (:doc (sut/resolve-symbol 'user `Thread/activeCount)))
          "Subsequent calls aren't affected, since there's no caching interference"))))
