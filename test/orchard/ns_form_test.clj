(ns orchard.ns-form-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
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
