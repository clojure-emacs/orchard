(ns orchard.java-test
  (:require
   [clojure.java.io :as io]
   [clojure.java.javadoc :as clojure.javadoc]
   [clojure.test :refer :all]
   [dynapath.util :as dp]
   [orchard.java :refer :all]
   [orchard.misc :as misc]))

(def jdk-parser? (or (>= misc/java-api-version 9) jdk-tools))

(deftest resources-test
  ;; If the JDK resources we wish to load dynamically are present on the file
  ;; system, test that we've resolved them and added them to the classpath.
  (testing "Resource loading correctness"
    (let [jdk-sources-path (jdk-find "src.zip")
          jdk-tools-path (jdk-find "tools.jar")
          classpath-urls (-> (.getContextClassLoader (Thread/currentThread))
                             (dp/all-classpath-urls)
                             (set))]
      (testing "of defined vars"
        (is (= jdk-sources jdk-sources-path))
        (is (= jdk-tools jdk-tools-path)))
      (testing "of dynamically added classpath entries"
        (is (= jdk-sources (classpath-urls jdk-sources)))
        (is (= jdk-tools (classpath-urls jdk-tools)))))))

(deftest source-info-test
  (let [resolve-src (comp (fnil io/resource "-none-") :file source-info)]
    (when jdk-parser?
      (testing "Source file resolution"
        (testing "for Clojure classes"
          (is (resolve-src 'clojure.lang.Obj))
          (is (resolve-src 'clojure.lang.Fn)))
        (when jdk-sources
          (testing "for JDK classes"
            (is (resolve-src 'java.lang.String))
            (is (resolve-src 'java.util.regex.Matcher))))
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
        (when jdk-sources
          (testing "for JDK classes"
            (is (-> (source-info 'java.util.Collection) :line)) ; interface
            (is (-> (source-info 'java.util.AbstractCollection) :line)) ; abstract class
            (is (-> (source-info 'java.lang.Thread$UncaughtExceptionHandler) :line)) ; nested interface
            (is (-> (source-info 'java.net.Authenticator$RequestorType) :line)) ; nested enum
            (is (-> (source-info 'java.sql.ClientInfoStatus) :line))))) ; top-level enum

      (testing "Source parsing"
        (testing "for Clojure classes"
          (is (-> (source-info 'clojure.lang.ExceptionInfo) :doc))
          (is (-> (get-in (source-info 'clojure.lang.BigInt)
                          [:members 'multiply])
                  first val :line)))
        (when jdk-sources
          (testing "for JDK classes"
            (is (-> (source-info 'java.util.AbstractCollection) :doc))
            (is (-> (get-in (source-info 'java.util.AbstractCollection)
                            [:members 'size])
                    first val :line))))))))

(deftest map-structure-test
  (when jdk-parser?
    (testing "Parsed map structure = reflected map structure"
      (let [cols #{:file :line :column :doc :argnames :argtypes :path}
            keys= #(= (set (keys (apply dissoc %1 cols)))
                      (set (keys %2)))
            c1 (class-info* 'clojure.lang.Compiler)
            c2 (with-redefs [source-info (constantly nil)]
                 (class-info* 'clojure.lang.Compiler))]
        ;; Class info
        (is (keys= c1 c2))
        ;; Members
        (is (keys (:members c1)))
        (is (= (keys (:members c1))
               (keys (:members c2))))
        ;; Member info
        (is (->> (map keys=
                      (vals (:members c1))
                      (vals (:members c2)))
                 (every? true?)))))))

(deftest class-info-test
  (let [c1 (class-info 'clojure.lang.Agent)
        c2 (class-info 'clojure.lang.Range$BoundsCheck)
        c3 (class-info 'not.actually.AClass)]
    (testing "Class"
      (when jdk-parser?
        (testing "source file"
          (is (string? (:file c1)))
          (is (io/resource (:file c1))))
        (testing "source file for nested class"
          (is (string? (:file c2)))
          (is (io/resource (:file c2)))))
      (testing "member info"
        (is (map? (:members c1)))
        (is (every? map? (vals (:members c1))))
        (is (apply (every-pred :name :modifiers)
                   (mapcat vals (vals (:members c1))))))
      (testing "doesn't throw on classes without dots in classname"
        (let [reified (binding [*ns* (create-ns 'foo)]
                        (clojure.core/eval
                         '(clojure.core/reify Object)))
              sym (symbol (.getName (class reified)))]
          (is (class-info sym))))
      (testing "that doesn't exist"
        (is (nil? c3))))))

(deftest member-info-test
  (let [m1 (member-info 'clojure.lang.PersistentHashMap 'assoc)
        m2 (member-info 'java.util.AbstractCollection 'non-existent-member)
        m3 (member-info 'not.actually.AClass 'nada)
        m4 (member-info 'java.awt.Point 'x)
        m5 (member-info 'java.lang.Class 'forName)
        m6 (member-info 'java.util.AbstractMap 'finalize)
        m7 (member-info 'java.util.HashMap 'finalize)]
    (testing "Member"
      (when jdk-parser?
        (testing "source file"
          (is (string? (:file m1)))
          (is (io/resource (:file m1))))
        (testing "line number"
          (is (number? (:line m1)))))
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
        (is (not= 'java.lang.Object (:class m7)))))))

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
                    cache (atom {})]
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
                      cache (atom {})]
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

(def javadoc-bases
  "Copied from clojure.java.javadoc. These are the base urls for
  javadocs from `clojure.java.javadoc/*core-java-api*`"
  {8 "http://docs.oracle.com/javase/8/docs/api/"
   9 "http://docs.oracle.com/javase/9/docs/api/"
   10 "http://docs.oracle.com/javase/10/docs/api/"
   11 "https://docs.oracle.com/en/java/javase/11/docs/api/java.base/"})

(deftest resolve-javadoc-path-test
  (let [get-url (comp resolve-javadoc-path (partial apply javadoc-url))]
    (when (= 8 misc/java-api-version)
      (testing "Java 8 javadocs resolve to the correct urls"
        (with-bindings {#'clojure.javadoc/*core-java-api* (javadoc-bases 8)}
          (with-redefs [misc/java-api-version 8
                        cache (atom {})]
            (are [class url] (= url (get-url class))
              ['java.lang.String]
              "https://docs.oracle.com/javase/8/docs/api/java/lang/String.html"

              ['java.lang.String 'contains nil]
              "https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#contains"

              ['java.lang.String 'contains ['java.lang.CharSequence]]
              "https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#contains-java.lang.CharSequence-")))))

    (when (>= misc/java-api-version 9)
      (testing "Java 9 javadocs resolve to the correct urls"
        (with-bindings {#'clojure.javadoc/*core-java-api* (javadoc-bases 9)}
          (with-redefs [misc/java-api-version 9
                        cache (atom {})]
            (testing "java.base modules resolve correctly"
              (are [class url] (= url (get-url class))
                ['java.lang.String]
                "https://docs.oracle.com/javase/9/docs/api/java/lang/String.html"

                ['java.lang.String 'contains nil]
                "https://docs.oracle.com/javase/9/docs/api/java/lang/String.html#contains"

                ['java.lang.String 'contains ['java.lang.CharSequence]]
                "https://docs.oracle.com/javase/9/docs/api/java/lang/String.html#contains-java.lang.CharSequence-"))))))

    ;; these tests require resolving module names so should only run on 11
    (when (= 11 misc/java-api-version)
      (testing "Java 11 javadocs resolve to the correct urls"
        (with-redefs [misc/java-api-version 11
                      cache (atom {})]
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
              "https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpRequest.html#newBuilder(java.net.URI)")))))))

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
      (testing "of classes/constructors"
        (is (= 'java.lang.String (:class (resolve-symbol ns 'String)))))
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
