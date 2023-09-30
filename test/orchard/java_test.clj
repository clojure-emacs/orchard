(ns orchard.java-test
  (:require
   [clojure.java.io :as io]
   [clojure.java.javadoc :as javadoc]
   [clojure.string :as string]
   [clojure.test :refer [are deftest is testing]]
   [orchard.java :as sut :refer [cache class-info class-info* javadoc-url jdk-tools member-info resolve-class resolve-javadoc-path resolve-member resolve-symbol resolve-type source-info]]
   [orchard.misc :as misc]
   [orchard.test.util :as util])
  (:import
   (mx.cider.orchard LruMap)))

(def jdk-parser? (or (>= misc/java-api-version 9) jdk-tools))

(javadoc/add-remote-javadoc "com.amazonaws." "http://docs.aws.amazon.com/AWSJavaSDK/latest/javadoc/")
(javadoc/add-remote-javadoc "org.apache.kafka." "https://kafka.apache.org/090/javadoc/")

(when util/has-enriched-classpath?
  (deftest source-info-test
    (let [resolve-src (comp (fnil io/resource "-none-") :file source-info)]
      (testing "Source file resolution"
        (testing "for Clojure classes"
          (is (resolve-src 'clojure.lang.Obj))
          (is (resolve-src 'clojure.lang.Fn)))
        (testing "for JDK classes"
          (is (resolve-src 'java.lang.String))
          (is (resolve-src 'java.util.regex.Matcher)))
        (testing "for non-existent classes"
          (is (not (resolve-src 'not.actually.AClass)))))

      (testing "Parse tree kinds"
        (testing "for Clojure classes"
          (is (-> (source-info 'clojure.lang.ISeq) :line)) ; interface
          (is (-> (source-info 'clojure.lang.AFn) :line)) ; abstract class
          (is (-> (source-info 'clojure.lang.APersistentMap$ValSeq) :line)) ; nested class
          (is (-> (source-info 'clojure.lang.Numbers$Ops) :line)) ; nested default interface
          (is (-> (source-info 'clojure.lang.Range$BoundsCheck) :line)) ; nested private interface
          (is (-> (source-info 'clojure.lang.Numbers$Category) :line))) ; nested enum
        (testing "for JDK classes"
          (is (-> (source-info 'java.util.Collection) :line)) ; interface
          (is (-> (source-info 'java.util.AbstractCollection) :line)) ; abstract class
          (is (-> (source-info 'java.lang.Thread$UncaughtExceptionHandler) :line)) ; nested interface
          (is (-> (source-info 'java.net.Authenticator$RequestorType) :line)) ; nested enum
          (is (-> (source-info 'java.sql.ClientInfoStatus) :line)))) ; top-level enum

      (testing "Source parsing"
        (testing "for Clojure classes"
          (is (-> (source-info 'clojure.lang.ExceptionInfo) :doc))
          (is (some-> (get-in (source-info 'clojure.lang.BigInt)
                              [:members 'multiply])
                      first val :line)))
        (testing "for JDK classes"
          (is (-> (source-info 'java.util.AbstractCollection) :doc))
          (is (some-> (get-in (source-info 'java.util.AbstractCollection)
                              [:members 'size])
                      first val :line)))))))

(deftest map-structure-test
  (testing "Parsed map structure = reflected map structure"
    (let [excluded-cols #{:file :line :column :doc :argnames :non-generic-argtypes :annotated-arglists
                          :doc-first-sentence-fragments :doc-fragments :doc-block-tags-fragments :argtypes :path :resource-url}
          keys= (fn [a b]
                  (is (= (set (keys (apply dissoc a excluded-cols)))
                         (set (keys (apply dissoc b excluded-cols))))))
          c1 (class-info* 'clojure.lang.Compiler)
          c2 (with-redefs [source-info (constantly nil)]
               (class-info* 'clojure.lang.Compiler))]
      ;; Class info
      (testing (str "Difference: "
                    (pr-str [(remove (set (keys c1)) (keys c2))
                             (remove (set (keys c2)) (keys c1))]))
        (is (keys= c1 c2))))))

(when util/has-enriched-classpath?
  (deftest class-info-test
    (let [c1 (class-info 'clojure.lang.Agent)
          c2 (class-info 'clojure.lang.Range$BoundsCheck)
          c3 (class-info 'not.actually.AClass)
          thread-class-info (class-info `Thread)]
      (when-not @@sut/parser-next-available?
        (throw @sut/parser-available-exception))
      (testing "Class"
        (testing "source file"
          (is (string? (:file c1)))
          (is (io/resource (:file c1))))
        (testing "source file for nested class"
          (is (string? (:file c2)))
          (is (io/resource (:file c2))))
        (testing "member info"
          (is (map? (:members c1)))
          (is (every? map? (vals (:members c1))))
          (let [members (mapcat vals (vals (:members c1)))]
            (assert (seq members))
            (doseq [m members]
              (is (contains? m :name))
              (assert (is (contains? m :modifiers))))))
        (testing "doesn't throw on classes without dots in classname"
          (let [reified (binding [*ns* (create-ns 'foo)]
                          (clojure.core/eval
                           '(clojure.core/reify Object)))
                sym (symbol (.getName (class reified)))]
            (is (class-info sym))))
        (testing "that doesn't exist"
          (is (nil? c3))))
      (when @@sut/parser-next-available?
        (testing "Doc fragments"
          (is (seq (:doc-fragments thread-class-info)))
          (is (seq (:doc-first-sentence-fragments thread-class-info))))))))

(when util/has-enriched-classpath?
  (deftest member-info-test
    (let [m1 (member-info 'clojure.lang.PersistentHashMap 'assoc)
          m2 (member-info 'java.util.AbstractCollection 'non-existent-member)
          m3 (member-info 'not.actually.AClass 'nada)
          m4 (member-info 'java.awt.Point 'x)
          m5 (member-info 'java.lang.Class 'forName)
          m6 (member-info 'java.util.AbstractMap 'finalize)
          m7 (member-info 'java.util.HashMap 'finalize)
          m8 (member-info `Thread 'resume)]
      (testing "Member"
        (testing "source file"
          (is (string? (:file m1)))
          (is (io/resource (:file m1))))
        (testing "line number"
          (is (number? (:line m1))))
        (testing "arglists"
          (is (seq? (:arglists m1)))
          (is (every? vector? (:arglists m1))))
        (testing "that doesn't exist"
          (is (nil? m2)))
        (testing "in a class that doesn't exist"
          (is (nil? m3)))
        (testing "that is a field"
          (is m4))
        (testing "that is static"
          (is m5))
        (testing "implemented on immediate superclass"
          (is (not= 'java.lang.Object (:class m6))))
        (testing "implemented on ancestor superclass"
          (is (not= 'java.lang.Object (:class m7)))
          (testing (-> m6 :doc pr-str)
            (is (-> m6 :doc (string/starts-with? "Called by the garbage collector on an object when garbage collection"))
                "Contains doc that is clearly defined in Object (the superclass)")))
        (when @@sut/parser-next-available?
          (testing "Doc fragments"
            (testing "For a field"
              (is (seq (:doc-fragments m4)))
              (is (seq (:doc-first-sentence-fragments m4))))

            (testing "For a method"
              (is (seq (:doc-fragments m8)))
              (is (seq (:doc-first-sentence-fragments m8))))))))))

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
    (testing "for Java < 11" ; JDK8 - JDK11
      (with-redefs [misc/java-api-version 8
                    cache (LruMap. 100)]
        (testing "of a class"
          (is (= (:javadoc (class-info 'java.lang.String))
                 "java/lang/String.html")))

        (testing "of a nested class"
          (is (= (:javadoc (class-info 'java.util.AbstractMap$SimpleEntry))
                 "java/util/AbstractMap.SimpleEntry.html")))

        (testing "of an interface"
          (is (= (:javadoc (class-info 'java.io.Closeable))
                 "java/io/Closeable.html")))

        (testing "of a class member"
          (testing "with no args"
            (is (= (:javadoc (member-info 'java.util.Random 'nextLong))
                   "java/util/Random.html#nextLong--")))
          (testing "with primitive args"
            (is (= (:javadoc (member-info 'java.util.Random 'setSeed))
                   "java/util/Random.html#setSeed-long-")))
          (testing "with object args"
            (is (= (:javadoc (member-info 'java.lang.String 'contains))
                   "java/lang/String.html#contains-java.lang.CharSequence-")))
          (testing "with array args"
            (is (= (:javadoc (member-info 'java.lang.Thread 'enumerate))
                   "java/lang/Thread.html#enumerate-java.lang.Thread:A-")))
          (testing "with multiple args"
            (is (= (:javadoc (member-info 'java.util.ArrayList 'subList))
                   "java/util/ArrayList.html#subList-int-int-")))
          (testing "with generic type erasure"
            (is (= (:javadoc (member-info 'java.util.Hashtable 'putAll))
                   "java/util/Hashtable.html#putAll-java.util.Map-"))))))

    ;; Java 11+ URLs require module information, which is only available on Java 9+.
    (when (>= misc/java-api-version 9)
      (testing "for Java 11+"
        (with-redefs [misc/java-api-version 11
                      cache (LruMap. 100)]
          (testing "of a class"
            (is (= (:javadoc (class-info 'java.lang.String))
                   "java.base/java/lang/String.html")))

          (testing "of a nested class"
            (is (= (:javadoc (class-info 'java.util.AbstractMap$SimpleEntry))
                   "java.base/java/util/AbstractMap.SimpleEntry.html")))

          (testing "of an interface"
            (is (= (:javadoc (class-info 'java.io.Closeable))
                   "java.base/java/io/Closeable.html")))

          (testing "of a class member"
            (testing "with no args"
              (is (= (:javadoc (member-info 'java.util.Random 'nextLong))
                     "java.base/java/util/Random.html#nextLong()")))
            (testing "with primitive args"
              (is (= (:javadoc (member-info 'java.util.Random 'setSeed))
                     "java.base/java/util/Random.html#setSeed(long)")))
            (testing "with object args"
              (is (= (:javadoc (member-info 'java.lang.String 'contains))
                     "java.base/java/lang/String.html#contains(java.lang.CharSequence)")))
            (testing "with array args"
              (is (= (:javadoc (member-info 'java.lang.Thread 'enumerate))
                     "java.base/java/lang/Thread.html#enumerate(java.lang.Thread[])")))
            (testing "with multiple args"
              (is (= (:javadoc (member-info 'java.util.ArrayList 'subList))
                     "java.base/java/util/ArrayList.html#subList(int,int)")))
            (testing "with generic type erasure"
              (is (= (:javadoc (member-info 'java.util.Hashtable 'putAll))
                     "java.base/java/util/Hashtable.html#putAll(java.util.Map)")))))))))

(deftest resolve-javadoc-path-test
  (let [get-url (comp resolve-javadoc-path (partial apply javadoc-url))]
    (testing "Java 8 javadocs resolve to the correct urls"
      (with-redefs [misc/java-api-version 8
                    cache (LruMap. 100)]
        (are [class url] (= url (get-url class))
          ['java.lang.String]
          "https://docs.oracle.com/javase/8/docs/api/java/lang/String.html"

          ['java.lang.String 'contains nil]
          "https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#contains"

          ['java.lang.String 'contains ['java.lang.CharSequence]]
          "https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#contains-java.lang.CharSequence-")))

    (when (>= misc/java-api-version 9)
      (testing "Java 9 javadocs resolve to the correct urls"
        (with-redefs [misc/java-api-version 9
                      cache (LruMap. 100)]
          (testing "java.base modules resolve correctly"
            (are [class url] (= url (get-url class))
              ['java.lang.String]
              "https://docs.oracle.com/javase/9/docs/api/java/lang/String.html"

              ['java.lang.String 'contains nil]
              "https://docs.oracle.com/javase/9/docs/api/java/lang/String.html#contains"

              ['java.lang.String 'contains ['java.lang.CharSequence]]
              "https://docs.oracle.com/javase/9/docs/api/java/lang/String.html#contains-java.lang.CharSequence-")))))

    ;; these tests require resolving module names so should only run on 11
    (when (= 11 misc/java-api-version)
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
               (get-url '[org.apache.kafka.clients.consumer.ConsumerConfig])))))
    (when (>= misc/java-api-version 11)
      (testing "Unrecognized java version doesn't blank out the javadocs"
        (with-redefs [misc/java-api-version 12345
                      cache (LruMap. 100)]
          (is (= "https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html"
                 (get-url ['java.lang.String]))))))))

(deftest class-resolution-test
  (let [ns (ns-name *ns*)]
    (testing "Class resolution"
      (testing "of resolvable classes"
        (is (= 'java.lang.String (:class (resolve-class ns 'String))))
        (is (= 'java.lang.String (:class (resolve-class ns 'java.lang.String)))))
      (testing "of non-resolvable 'classes'"
        (is (nil? (resolve-class ns 'NothingHere)))
        (is (nil? (resolve-class ns 'not.actually.AClass))))
      (testing "of things that aren't classes"
        (is (nil? (resolve-class ns 'assoc)))
        (is (nil? (resolve-class ns 'clojure.core)))))))

(deftest member-resolution-test
  (let [ns (ns-name *ns*)]
    (testing "Member resolution"
      (testing "of instance members"
        (is (every? #(= 'toString (:member %))
                    (resolve-member ns 'toString))))
      (testing "of non-members"
        (is (empty? (resolve-member ns 'notAMember)))))))

(deftest symbol-resolution-test
  (let [ns (ns-name *ns*)]
    (testing "Symbol resolution"
      (testing "of unambiguous instance members"
        (is (= 'java.lang.SecurityManager
               (:class (resolve-symbol ns 'checkPackageDefinition)))))
      (testing "of candidate instance members"
        (is (every? #(= 'toString (:member %))
                    (vals (:candidates (resolve-symbol ns 'toString))))))
      (testing "of static methods"
        (is (= 'forName (:member (resolve-symbol ns 'Class/forName)))))
      (testing "of static fields"
        (is (= 'TYPE (:member (resolve-symbol ns 'Void/TYPE)))))

      (testing "equality of qualified vs unqualified"
        (testing "classes"
          (is (= (resolve-symbol ns 'java.lang.String)
                 (resolve-symbol ns 'String))))
        (testing "constructors"
          (is (= (resolve-symbol ns 'java.lang.Exception.)
                 (resolve-symbol ns 'Exception.))))
        (testing "static methods"
          (is (= (resolve-symbol ns 'java.lang.Class/forName)
                 (resolve-symbol ns 'Class/forName))))
        (testing "static fields"
          (is (= (resolve-symbol ns 'java.lang.Void/TYPE)
                 (resolve-symbol ns 'Void/TYPE)))))

      (testing "equality of dotted"
        (testing "constructor syntax"
          (is (= (resolve-symbol ns 'Exception)
                 (resolve-symbol ns 'Exception.))))
        (testing "method syntax"
          (is (= (resolve-symbol ns 'toString)
                 (resolve-symbol ns '.toString)))))

      (testing "of things that shouldn't resolve"
        (is (nil? (resolve-symbol ns 'MissingUnqualifiedClass)))
        (is (nil? (resolve-symbol ns 'missing.qualified.Class)))
        (is (nil? (resolve-symbol ns 'MissingUnqualifiedCtor.)))
        (is (nil? (resolve-symbol ns 'missing.qualified.Ctor.)))
        (is (nil? (resolve-symbol ns 'MissingUnqualified/staticMethod)))
        (is (nil? (resolve-symbol ns 'missing.Qualified/staticMethod)))
        (is (nil? (resolve-symbol ns 'missingMethod)))
        (is (nil? (resolve-symbol ns '.missingDottedMethod)))
        (is (nil? (resolve-symbol ns '.random.bunch/of$junk)))))))

(deftest type-resolution-test
  (testing "Type resolution"
    (testing "of Java classes/constructors in any namespace"
      (is (= 'java.lang.String (:class (resolve-type (ns-name *ns*) 'String)))))
    (testing "of deftype in clojure.core"
      (is (= 'clojure.core.Eduction (:class (resolve-type 'clojure.core 'Eduction)))))))

(defn- replace-last-dot [^String s]
  (if (re-find #"(.*\.)" s)
    (str (second (re-matches #"(.*)(\..*)" s))
         "$"
         (subs s (inc (.lastIndexOf s "."))))
    s))

(defn class-corpus []
  {:post [(> (count %)
             50)]}
  (->> (util/imported-classes 'clojure.core)
       (into ['java.util.Map 'java.io.File])
       (into (util/imported-classes (-> ::_ namespace symbol)))
       ;; Remove classes without methods:
       (remove (some-fn
                #{`ThreadDeath
                  `Void
                  `RuntimePermission
                  'clojure.core.Vec
                  'clojure.core.VecNode
                  'clojure.core.VecSeq
                  'clojure.core.ArrayChunk
                  'clojure.core.Eduction}
                (fn [s]
                  (-> s str Class/forName .isInterface))
                (fn [s]
                  (->> s str (re-find #"(Exception|Error)$")))))))

(defn extract-method-arities [info]
  (->> (-> info
           :members
           vals)
       (map vals)
       (reduce into)
       ;; Only methods (and not fields) have arglists:
       (filter :returns)))

(when util/has-enriched-classpath?
  (deftest reflect-and-source-info-match
    (testing "reflect and source info structurally match, allowing a meaningful deep-merge of both"
      (let [extract-arities (fn [info]
                              (->> info :members vals (map keys) (reduce into)
                                   (remove nil?) ;; fields
                                   (sort-by pr-str)))]
        (doseq [class-symbol (class-corpus)
                :let [f @(requiring-resolve 'orchard.java.parser-next/source-info)
                      source-info (f class-symbol)
                      reflect-info (sut/reflect-info (#'sut/reflection-for (eval class-symbol)))
                      arities-from-source (extract-arities source-info)
                      arities-from-reflector (extract-arities reflect-info)]]
          (testing class-symbol
            (assert (= (count arities-from-source)
                       (count arities-from-reflector))
                    [class-symbol
                     (count arities-from-source)
                     (count arities-from-reflector)
                     :source (sort-by pr-str arities-from-source)
                     :reflector (sort-by pr-str arities-from-reflector)])
            (assert (or (pos? (count arities-from-source))
                        (pos? (count arities-from-reflector)))
                    class-symbol)
            (doall (map-indexed (fn [i x]
                                  (is (= x
                                         (nth arities-from-reflector i)))
                                  (doseq [s x
                                          :let [s (-> s str (string/replace "[]" ""))]]
                                    (assert (is (or (#{"byte" "short" "int" "long" "float" "double" "char" "boolean" "void"}
                                                     s)
                                                    (try
                                                      (Class/forName s)
                                                      (catch Exception _
                                                        (Class/forName (replace-last-dot s)))))
                                                "The "))))
                                arities-from-source))
            (assert (is (= arities-from-source
                           arities-from-reflector)))

            (let [arities-data (extract-method-arities (misc/deep-merge reflect-info source-info))
                  all-argnames (map :argnames arities-data)]
              (assert (pos? (count all-argnames)))
              (is (not-any? nil? all-argnames)
                  "The deep-merge went ok"))))))))

(when util/has-enriched-classpath?
  (deftest annotated-arglists-test
    (doseq [class-symbol (class-corpus)
            :let [info (sut/class-info* class-symbol)
                  arities (extract-method-arities info)
                  all-annotated-arglists (map :annotated-arglists arities)]]
      (testing class-symbol
        (assert (pos? (count all-annotated-arglists))
                class-symbol)
        (doseq [s all-annotated-arglists]
          (assert (is (string? s)))
          (testing s
            (is (re-find #"\^.*\[" s))
            ;; Assert that the format doesn't include past bugs:
            (is (not (string/includes? s "<")))
            (is (not (string/includes? s "^Object java.lang.Object")))
            (is (not (string/includes? s "^Object Object")))
            (is (not (string/includes? s "^function.Function java.util.function.Function")))
            (is (not (string/includes? s "^java.util.function.Function java.util.function.Function")))
            (assert (is (not (string/includes? s "java.lang"))))))))))
