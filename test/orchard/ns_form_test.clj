(ns orchard.ns-form-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [are deftest is testing]]
   [orchard.ns-form :as sut])
  (:import
   (clojure.lang TaggedLiteral)
   (java.io File StringReader)))

(defn- read-str
  ([s] (sut/read-ns-form (StringReader. s)))
  ([s platform] (sut/read-ns-form (StringReader. s) platform)))

(deftest read-ns-form-test
  (testing "the first ns form"
    (is (= '(ns foo.bar (:require [clojure.string :as str]))
           (read-str "(ns foo.bar (:require [clojure.string :as str])) (ns other)"))))

  (testing "forms before the ns form are skipped"
    (is (= '(ns foo.bar) (read-str ";; comment\n(set! *warn-on-reflection* true)\n(ns foo.bar)"))))

  (testing "a qualified ns"
    (is (= '(clojure.core/ns foo.bar) (read-str "(clojure.core/ns foo.bar)"))))

  (testing "no ns form"
    (is (nil? (read-str "(defn foo [])")))
    (is (nil? (read-str ""))))

  (testing "something that doesn't read"
    (is (nil? (read-str "(ns foo.bar")))
    (is (nil? (sut/read-ns-form (File. "does/not/exist.clj")))))

  (testing "nothing gets evaluated"
    (is (nil? (read-str "#=(java.lang.System/exit 1) (ns foo.bar)"))))

  (testing "tagged literals come back as they're written"
    (let [form (read-str "(ns foo.bar {:x #js {:a 1}})")]
      (is (instance? TaggedLiteral (:x (nth form 2))))))

  (testing "a splice that isn't a list or a vector doesn't read"
    (is (nil? (read-str "(ns foo.bar (:require #?@(:clj foo)))" :clj)))
    (is (nil? (read-str "(ns foo.bar (:require #?@(:clj \"ab\")))" :clj))))

  (testing "something nested too deep doesn't read"
    (is (nil? (read-str (str "(ns foo.bar " (apply str (repeat 100000 "[")))))))

  (testing "data readers don't run, except for #inst and #uuid"
    (binding [*data-readers* {'foo/bar (fn [_] (throw (Exception. "ran")))}]
      (is (instance? TaggedLiteral (:x (nth (read-str "(ns foo.bar {:x #foo/bar 1})") 2)))))
    (is (instance? java.util.Date (:x (nth (read-str "(ns foo.bar {:x #inst \"2020-01-01\"})") 2)))))

  (testing "a file"
    (let [f (File/createTempFile "ns_form_test" ".clj")]
      (try
        (spit f "(ns foo.bar\n  (:require [clojure.set :as set]))\n")
        (is (= '(ns foo.bar (:require [clojure.set :as set])) (sut/read-ns-form f)))
        (is (= '(ns foo.bar (:require [clojure.set :as set])) (sut/read-ns-form (io/as-url f))))
        (finally (.delete f))))))

(def ^:private cljc-ns
  "(ns foo.bar
     (:require
      [clojure.string :as str]
      #?(:clj [clojure.java.io :as io] :cljs [goog.string :as gstr])
      #?@(:clj [[clojure.set :as set]] :default [[cljs.set :as set] [cljs.walk]])
      #?(:cljs [only.cljs])))")

(deftest for-platform-test
  (testing "reader conditionals stay in the form when no platform is given"
    (is (reader-conditional? (-> (read-str cljc-ns) (nth 2) (nth 2)))))

  (testing "the branch for the platform"
    (is (= '(ns foo.bar (:require [clojure.string :as str]
                                  [clojure.java.io :as io]
                                  [clojure.set :as set]))
           (read-str cljc-ns :clj)))
    (is (= '(ns foo.bar (:require [clojure.string :as str]
                                  [goog.string :as gstr]
                                  [cljs.set :as set]
                                  [cljs.walk]
                                  [only.cljs]))
           (read-str cljc-ns :cljs))))

  (testing "the same as resolving a form read without a platform"
    (let [form (read-str cljc-ns)]
      (is (= (read-str cljc-ns :clj) (sut/for-platform form :clj)))
      (is (= (read-str cljc-ns :cljs) (sut/for-platform form :cljs)))))

  (testing "a conditional around the whole ns form"
    (let [s "#?(:clj (ns foo.clj) :cljs (ns foo.cljs))"]
      (is (= '(ns foo.clj) (read-str s :clj)))
      (is (= '(ns foo.cljs) (read-str s :cljs)))
      (is (nil? (read-str "#?(:cljs (ns foo.cljs))" :clj)))))

  (testing "a separate conditional ns form for each platform"
    (let [s "#?(:clj (ns foo.clj))\n#?(:cljs (ns foo.cljs))"]
      (is (= '(ns foo.clj) (read-str s :clj)))
      (is (= '(ns foo.cljs) (read-str s :cljs)))))

  (testing "a conditional with something else for the platform"
    (is (nil? (read-str "#?(:clj (ns foo.clj) :cljs (do-something))" :cljs))))

  (testing "nested splices"
    (is (= '(ns foo (:require a b c))
           (read-str "(ns foo (:require #?@(:clj [#?@(:clj [a b]) c])))" :clj))))

  (testing "metadata survives"
    (is (= {:doc "x"} (-> (read-str "(ns ^{:doc \"x\"} foo.bar)" :cljs) second meta (select-keys [:doc])))))

  (testing "maps and sets"
    (is (= '(ns foo {:a 1, :b #{2}})
           (read-str "(ns foo {:a #?(:clj 1 :cljs 0), :b #{#?(:clj 2)}})" :clj)))))

(deftest libspecs-test
  (testing "plain names, vectors and strings"
    (is (= '[{:ns foo.a, :clause :require}
             {:ns foo.b, :as b, :refer [x y], :clause :require}
             {:ns "react", :as react, :clause :require}
             {:ns foo.c, :as-alias c, :clause :require}]
           (sut/libspecs '(ns x (:require foo.a [foo.b :as b :refer [x y]] ["react" :as react]
                                          (foo.c :as-alias c)))))))

  (testing "prefix lists"
    (is (= '[{:ns clojure.set, :clause :require}
             {:ns clojure.string, :as str, :clause :require}
             {:ns clojure.walk, :clause :require}]
           (sut/libspecs '(ns x (:require (clojure set [string :as str]) [clojure walk]))))))

  (testing "a one-element vector isn't a prefix list"
    (is (= '[{:ns clojure.set, :clause :require}]
           (sut/libspecs '(ns x (:require [clojure.set]))))))

  (testing "flags are skipped"
    (is (= '[{:ns foo.a, :clause :require}]
           (sut/libspecs '(ns x (:require foo.a :reload))))))

  (testing "the other clauses, and a symbol for the clause name"
    (is (= '[{:ns foo.a, :clause :use}
             {:ns foo.b, :only [z], :clause :use}
             {:ns foo.m, :as m, :clause :require-macros}
             {:ns foo.n, :only [n], :clause :use-macros}
             {:ns foo.r, :clause :require}]
           (sut/libspecs '(ns x "doc" {:author "me"}
                              (:use foo.a [foo.b :only [z]])
                              (:require-macros [foo.m :as m])
                              (:use-macros [foo.n :only [n]])
                              (require foo.r)
                              (:import (java.util Date))
                              (:gen-class))))))

  (testing "ClojureScript's :require-global"
    (is (= '[{:ns Math, :as m, :clause :require-global}]
           (sut/libspecs '(ns x (:require-global [Math :as m]))))))

  (testing "no clauses"
    (is (empty? (sut/libspecs '(ns x))))))

(deftest aliases-test
  (is (= '{str clojure.string, set clojure.set, c foo.c, react "react", m foo.m}
         (sut/aliases '(ns x (:require [clojure.string :as str] (clojure [set :as set] walk)
                                       [foo.c :as-alias c] ["react" :as react])
                           (:require-macros [foo.m :as m])))))

  (testing "what doesn't make sense as an alias is left out"
    (is (empty? (sut/aliases '(ns x (:require [clojure.string :as :refer [x]] [1 :as kk] (:foo :as kw))))))))

(deftest imports-test
  (is (= '[java.util.Date java.util.UUID java.io.File java.time.Instant goog.string.StringBuffer]
         (sut/imports '(ns x (:import (java.util Date UUID) java.io.File [java.time Instant])
                           (:require foo)
                           (:import [goog.string StringBuffer]))))))

(defn- unused-in
  ([s] (unused-in s :clj))
  ([s platform] (sut/unused (StringReader. s) platform)))

(defn- unused-libspecs
  ([s] (unused-libspecs s :clj))
  ([s platform] (:libspecs (unused-in s platform))))

(deftest unused-libspecs-test
  (testing "an alias nothing uses"
    (is (= [{:ns 'clojure.string, :clause :require, :remove true}]
           (unused-libspecs "(ns foo (:require [clojure.string :as str])) (str \"a\" 1)"))))

  (testing "an alias in use"
    (are [code] (= [] (unused-libspecs (str "(ns foo (:require [clojure.string :as str])) " code)))
      "(str/join [])"
      "::str/kw"
      "{::str/keys [a]}"
      "#::str{:a 1}"
      "`(str/join [])"
      "#?(:cljs (str/join []))"
      "#js {:a (str/join [])}"
      "(comment (str/join []))"))

  (testing "an alias whose namespace is still used fully qualified"
    (is (= [{:ns 'clojure.string, :clause :require, :as 'str}]
           (unused-libspecs "(ns foo (:require [clojure.string :as str])) (clojure.string/join [])"))))

  (testing "referred names"
    (is (= [{:ns 'clojure.string, :clause :require, :refer '[split]}]
           (unused-libspecs "(ns foo (:require [clojure.string :refer [join split]])) (join [])")))
    (is (= [{:ns 'clojure.string, :clause :require, :remove true}]
           (unused-libspecs "(ns foo (:require [clojure.string :refer [join]])) (str/join [])")))
    (are [code] (= [] (unused-libspecs (str "(ns foo (:require [clojure.string :refer [join]])) " code)))
      "`(join [])"
      "#'join"
      "(let [join 1] join)"))

  (testing "an alias and referred names"
    (is (= [{:ns 'clojure.string, :clause :require, :as 'str}]
           (unused-libspecs "(ns foo (:require [clojure.string :as str :refer [join]])) (join [])")))
    (is (= [{:ns 'clojure.string, :clause :require, :refer '[join]}]
           (unused-libspecs "(ns foo (:require [clojure.string :as str :refer [join]])) (str/split \"\" #\"a\")")))
    (is (= [{:ns 'clojure.string, :clause :require, :as 'str, :refer '[join]}]
           (unused-libspecs "(ns foo (:require [clojure.string :as str :refer [join]])) clojure.string/split"))))

  (testing ":as-alias"
    (is (= [{:ns 'foo.spec, :clause :require, :remove true}]
           (unused-libspecs "(ns foo (:require [foo.spec :as-alias spec]))")))
    (is (= [] (unused-libspecs "(ns foo (:require [foo.spec :as-alias spec])) ::spec/id")))
    (testing "doesn't load its namespace, so it goes even when that's used"
      (is (= [{:ns 'foo.spec, :clause :require, :remove true}]
             (unused-libspecs "(ns foo (:require [foo.spec :as-alias spec])) foo.spec/x")))))

  (testing "what's left alone"
    (are [libspec] (= [] (unused-libspecs (str "(ns foo (:require " libspec "))")))
      "clojure.string"
      "[clojure.string]"
      "\"react\""
      "[clojure.string :refer :all]"
      "[clojure.string :refer [join] :rename {join j}]"
      "[clojure [string :as str] [set :as set]]"
      "#?(:clj [clojure.string :as str])"
      "[clojure.string :as]"
      "[clojure.string :as \"str\"]"
      "[clojure.string :as str] [clojure.string :refer [join]]")
    (is (= [] (unused-libspecs "(ns foo (:use [clojure.string :only [join]]))"))))

  (testing "a namespace mentioned some other way only loses its alias"
    (are [code] (= [{:ns 'foo-bar.records, :clause :require, :as 'r}]
                   (unused-libspecs (str "(ns foo (:require [foo-bar.records :as r])) " code)))
      "(foo_bar.records.Rec. 1)"
      "(instance? foo_bar.records.Rec x)"
      "(foo_bar.records.Rec/create {})"
      "(defn f [^foo_bar.records.Rec x])"
      "(ns-publics 'foo-bar.records)"
      "(s/valid? :foo-bar.records/user {})"))

  (testing "a referred name used as a qualifier or with a dot"
    (are [code] (= [] (unused-libspecs (str "(ns foo (:require [goog.events :refer [EventTarget]])) " code)))
      "(EventTarget/foo)"
      "EventTarget.prototype"))

  (testing "an empty refer list is a bare require"
    (is (= [] (unused-libspecs "(ns foo (:require [foo.side :refer []]))"))))

  (testing "names referred twice"
    (is (= [{:ns 'foo.core, :clause :require, :refer '[b]}]
           (unused-libspecs "(ns foo (:require [foo.core :refer [a b b]])) (a)"))))

  (testing "the ns form's own symbols don't count"
    (is (= [{:ns 'clojure.string, :clause :require, :remove true}]
           (unused-libspecs "(ns foo (:require [clojure.string :as str] [str.core :as core])) core/x"))))

  (testing "ClojureScript"
    (is (= [{:ns "react", :clause :require, :remove true}]
           (unused-libspecs "(ns foo (:require [\"react\" :as react]))")))
    (are [code] (= [] (unused-libspecs (str "(ns foo (:require [\"react\" :as react])) " code)))
      "(react/createElement)"
      "(.createElement react \"div\")"
      "(react. 1)")
    (testing "a namespace's alias used as a value"
      (is (= [] (unused-libspecs "(ns foo (:require [react :as react])) (.createElement react)" :cljs)))
      (is (= [] (unused-libspecs "(ns foo (:require [react :as react])) (.createElement react)" nil)))
      (is (= [{:ns 'react, :clause :require, :remove true}]
             (unused-libspecs "(ns foo (:require [react :as react])) (.createElement react)" :cljc))))
    (testing "a Closure namespace used fully qualified"
      (is (= [{:ns 'goog.string, :clause :require, :as 'gstr}]
             (unused-libspecs "(ns foo (:require [goog.string :as gstr])) (goog.string.format \"%s\" 1)" :cljs))))
    (is (= [{:ns 'foo.macros, :clause :require-macros, :remove true}]
           (unused-libspecs "(ns foo (:require-macros [foo.macros :as m]))")))
    (is (= [{:ns 'foo.core, :clause :require, :refer-macros '[b]}]
           (unused-libspecs "(ns foo (:require [foo.core :refer-macros [a b]])) (a)"))))

  (testing "a namespace whose classes get imported stays"
    (is (= [{:ns 'foo.records, :clause :require, :as 'r}]
           (unused-libspecs "(ns foo (:require [foo.records :as r]) (:import (foo.records Rec))) (Rec. 1)")))
    (is (= [{:ns 'foo-bar.records, :clause :require, :as 'r}]
           (unused-libspecs "(ns foo (:require [foo-bar.records :as r]) (:import foo_bar.records.Rec)) Rec")))))

(deftest unused-imports-test
  (testing "classes nothing uses"
    (is (= '[java.util.List java.io.File]
           (:imports (unused-in "(ns foo (:import (java.util Date List) java.io.File)) (Date.)")))))

  (testing "classes in use"
    (are [code] (= [] (:imports (unused-in (str "(ns foo (:import java.io.File)) " code))))
      "(File. \"a\")"
      "File/separator"
      "(defn f [^File x])"
      "(fn ^File [])"
      "`(File. \"a\")"
      "`File/separator"
      "(catch File e)"))

  (testing "a string type hint"
    (are [code] (= [] (:imports (unused-in (str "(ns foo (:import java.io.File)) " code))))
      "(defn f [^\"File\" x])"
      "(defn f ^\"File\" [])"
      "(defn f [^{:tag \"File\"} x])"
      "(defmacro m [] `(fn [^\"File\" x#] (.getName x#)))"))

  (testing "a fully qualified use doesn't need the import"
    (is (= '[java.io.File] (:imports (unused-in "(ns foo (:import java.io.File)) java.io.File/separator")))))

  (testing "but in ClojureScript it does"
    (is (= [] (:imports (unused-in "(ns foo (:import [goog.events EventType])) goog.events.EventType.CLICK" :cljs))))
    (is (= [] (:imports (unused-in "(ns foo (:import [goog.events EventType])) goog.events.EventType.CLICK" nil))))
    (is (= [] (:imports (unused-in "(ns foo (:import [goog.events EventType])) EventType.CLICK" :cljs)))))

  (testing ":gen-class can use a class"
    (is (= [] (:imports (unused-in "(ns foo (:import (javax.swing JFrame)) (:gen-class :extends JFrame))")))))

  (testing "a class imported twice is left alone"
    (is (= [] (:imports (unused-in "(ns foo (:import java.io.File (java.io File)))"))))))

(deftest unused-test
  (testing "no ns form"
    (is (nil? (unused-in "(defn foo [])")))
    (is (nil? (unused-in "::str/kw")))
    (is (nil? (unused-in "#?(:clj (ns foo) :cljs (ns foo))"))))

  (testing "nothing unused"
    (is (= {:libspecs [], :imports []} (unused-in "(ns foo)"))))

  (testing "something that doesn't read"
    (is (thrown? Exception (unused-in "(ns foo (:require [clojure.string :as str])) (str/join")))
    (is (thrown? Exception (unused-in "(ns foo) #=(java.lang.System/exit 1)")))
    (testing "a line numbering reader keeps its line"
      (let [reader (doto (clojure.lang.LineNumberingPushbackReader. (StringReader. "(ns foo)\n(str/join []))"))
                     (.setLineNumber 10))]
        (is (= 11 (try (sut/unused reader)
                       (catch Exception e (:clojure.error/line (ex-data e))))))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"nested too deep"
                          (unused-in (str "(ns foo) " (apply str (repeat 100000 "["))))))))
