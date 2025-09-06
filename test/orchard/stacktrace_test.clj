(ns orchard.stacktrace-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [clojure.test :refer [are deftest is testing]]
   [matcher-combinators.matchers :as matchers]
   [orchard.stacktrace :as sut]
   [orchard.test.util :refer [is+]]))

;; # Utils

(defmacro catch-and-analyze [form]
  `(try ~form (catch Exception e# (sut/analyze e#))))

(defn causes
  [form]
  (catch-and-analyze (eval form)))

(defn stack-frames
  [form]
  (-> (catch-and-analyze (eval form))
      first :stacktrace))

;; ## Test fixtures

(def form1 '(throw (ex-info "oops" {:x 1} (ex-info "cause" {:y 2}))))
(def form2 '(let [^long num "2"] ;; Type hint to make eastwood happy
              (defn oops [] (+ 1 num))
              (oops)))
(def form3 '(not-defined))
(def form4 '(divi 1 0))

(def frames1 (stack-frames form1))
(def frames2 (stack-frames form2))
(def frames4 (stack-frames form4))
(def causes1 (causes form1))
(def causes2 (causes form2))
(def causes3 (causes form3))

(def email-regex
  #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$")

(s/check-asserts true)
(s/def ::email-type
  (s/and string? #(re-matches email-regex %)))

(s/def ::first-name string?)
(s/def ::last-name string?)
(s/def ::email ::email-type)

(s/def ::person
  (s/keys :req [::first-name ::last-name ::email]
          :opt [::phone]))

(def steven {::first-name "Steven"
             ::last-name  "Hawking"
             ::email      "n/a"})

(def spec-causes (causes `(s/assert ::person steven)))

(deftest spec-assert-stacktrace-test
  (testing "Spec assert components"
    (is+ [{:stacktrace some?
           :message some?
           :spec {:spec some?
                  :value string?
                  :problems [{:in some?
                              :val some?
                              :predicate some?
                              :spec some?
                              :at some?}]}}]
         spec-causes)))

(deftest stacktrace-frames-test
  (testing "File types"
    ;; Should be clj and java only.
    (is (= #{:clj :java} (set (map :type frames1))))
    (is (= #{:clj :java} (set (map :type frames2)))))

  (testing "Full file mappings"
    (is+ (matchers/seq-of {:file-url #".+!/clojure/core.clj$"})
         (filter #(= "clojure.core" (:ns %)) frames1))
    (is+ (matchers/seq-of {:file-url #"^file:/.+"})
         (filter #(some->> (:ns %) (re-find #"^orchard")) frames1)))

  (testing "Clojure ns, fn, and var"
    ;; All Clojure frames should have non-nil :ns :fn and :var attributes.
    (is+ (matchers/seq-of {:ns some?, :fn some?, :var some?})
         (filter #(= :clj (:type %)) frames1))
    (is+ (matchers/seq-of {:ns some?, :fn some?, :var some?})
         (filter #(= :clj (:type %)) frames2)))

  (testing "Clojure name demunging"
    ;; Clojure fn names should be free of munging characters.
    (is+ (matchers/seq-of {:fn (matchers/mismatch #"[_$]|(--\d+)")})
         (filter :fn frames1))
    (is+ (matchers/seq-of {:fn (matchers/mismatch #"[_$]|(--\d+)")})
         (filter :fn frames2))))

(deftest stacktrace-frame-flags-test
  (testing "Flags"
    (testing "for file type"
      ;; Every frame should have its file type added as a flag.
      (is (every? #(contains? (:flags %) (:type %)) frames1))
      (is (every? #(contains? (:flags %) (:type %)) frames2)))

    (testing "for tooling"
      ;; Tooling frames are classes named with 'clojure' or 'nrepl',
      ;; or are java thread runners...or calls made from these.
      (is (some #(re-find #"(clojure|nrepl|run)" (:name %))
                (filter (comp :tooling :flags) frames1)))
      (is (some #(re-find #"(clojure|nrepl|run)" (:name %))
                (filter (comp :tooling :flags) frames2))))

    (testing "for project"
      (is (seq (filter (comp :project :flags) frames4))))

    (testing "flags duplicate fns even if method names differ"
      (is+ [{:fn "foo", :flags (matchers/mismatch (matchers/embeds #{:dup}))}
            {:fn "foo", :flags (matchers/embeds #{:dup})}
            {:fn "eval12444", :flags (matchers/mismatch (matchers/embeds #{:dup}))}
            {:fn "eval12444", :flags (matchers/embeds #{:dup})}]
           (#'sut/analyze-stacktrace-data
            '[[orchard.stacktrace_test$foo invokeStatic "NO_SOURCE_FILE" 376]
              [orchard.stacktrace_test$foo invoke "NO_SOURCE_FILE" 375]
              [orchard.stacktrace_test$eval12444 invokeStatic "NO_SOURCE_FILE" 378]
              [orchard.stacktrace_test$eval12444 invoke "NO_SOURCE_FILE" 378]])))))

(deftest exception-causes-test
  (testing "Exception cause unrolling"
    (is (= 2 (count causes1)))
    (is (= 1 (count causes2))))
  (testing "Exception data"
    ;; If ex-data is present, the cause should have a :data attribute.
    (is (:data (first causes1)))
    (is (not (:data (first causes2))))))

(deftest compilation-errors-test
  (testing "first cause of compiler exception looks like this"
    (is+ {:message #"Syntax error compiling at \(.*orchard/stacktrace_test\.clj:"}
         (first causes3)))

  (testing "extract-location with location-data already present"
    (is (= {:class    "clojure.lang.Compiler$CompilerException"
            :location {:clojure.error/line 1
                       :clojure.error/column 42
                       :clojure.error/source "/foo/bar/baz.clj"
                       :clojure.error/phase :macroexpand
                       :clojure.error/symbol 'clojure.core/let},
            :message  "Syntax error macroexpanding clojure.core/let at (1:1)."
            :file     "/foo/bar/baz.clj"
            :file-url nil
            :path     "/foo/bar/baz.clj"
            :line     1
            :column   42}
           (#'sut/extract-location {:class    "clojure.lang.Compiler$CompilerException"
                                    :location {:clojure.error/line   1
                                               :clojure.error/column 42
                                               :clojure.error/source "/foo/bar/baz.clj"
                                               :clojure.error/phase  :macroexpand
                                               :clojure.error/symbol 'clojure.core/let}
                                    :message  "Syntax error macroexpanding clojure.core/let at (1:1)."})))))

(deftest analyze-cause-test
  (testing "check that location-data is returned"
    (let [e (ex-info "wat?" {:clojure.error/line 1
                             :clojure.error/column 42
                             :clojure.error/source "/foo/bar/baz.clj"
                             :clojure.error/phase :macroexpand
                             :clojure.error/symbol 'clojure.core/let})
          cause (first (sut/analyze e))]
      (is (= {:clojure.error/line 1
              :clojure.error/column 42
              :clojure.error/source "/foo/bar/baz.clj"
              :clojure.error/phase :macroexpand
              :clojure.error/symbol 'clojure.core/let}
             (:location cause))))))

(deftest ex-triage-test
  (testing "compilation errors that can be triaged contain :triage message"
    (is (= "[a] - failed: even-number-of-forms? in: [0] at: [:bindings] spec: :clojure.core.specs.alpha/bindings"
           (str/trim (:triage (first (catch-and-analyze (eval '(let [a]))))))))))

(deftest test-analyze-throwable
  (testing "shape of analyzed throwable"
    (is+ [;; first cause
          {:class "clojure.lang.ExceptionInfo"
           :message "BOOM-1"
           :data "{:boom \"1\"}"
           :stacktrace [{:name "clojure.lang.AFn/applyToHelper"
                         :file "AFn.java"
                         :line 156
                         :class "clojure.lang.AFn"
                         :method "applyToHelper"
                         :type :java
                         :flags #{:java :tooling}}
                        {:class "clojure.lang.AFn"
                         :file "AFn.java"
                         :flags #{:java :tooling}
                         :line 144
                         :method "applyTo"
                         :name "clojure.lang.AFn/applyTo"
                         :type :java}
                        ;; 5 frames in total
                        some? some? some?]}
          ;; second cause
          {:class "clojure.lang.ExceptionInfo"
           :message "BOOM-2"
           :data "{:boom \"2\"}"
           :stacktrace [{:name "clojure.lang.AFn/applyToHelper"
                         :file "AFn.java"
                         :class "clojure.lang.AFn"
                         :line 160
                         :method "applyToHelper"
                         :type :java
                         :flags #{:java :tooling}}]}
          ;; third cause
          {:class "clojure.lang.ExceptionInfo"
           :message "BOOM-3"
           :data "{:boom \"3\"}"
           :stacktrace [{:name "clojure.lang.AFn/applyToHelper"
                         :file "AFn.java"
                         :class "clojure.lang.AFn"
                         :line 156
                         :method "applyToHelper"
                         :type :java
                         :flags #{:java :tooling}}]}]
         (sut/analyze
          '{:via
            [{:type clojure.lang.ExceptionInfo
              :message "BOOM-1"
              :data {:boom "1"}
              :at [clojure.lang.AFn applyToHelper "AFn.java" 160]}
             {:type clojure.lang.ExceptionInfo
              :message "BOOM-2"
              :data {:boom "2"}
              :at [clojure.lang.AFn applyToHelper "AFn.java" 160]}
             {:type clojure.lang.ExceptionInfo
              :message "BOOM-3"
              :data {:boom "3"}
              :at [clojure.lang.AFn applyToHelper "AFn.java" 156]}]
            :trace
            [[clojure.lang.AFn applyToHelper "AFn.java" 156]
             [clojure.lang.AFn applyTo "AFn.java" 144]
             [clojure.lang.Compiler$InvokeExpr eval "Compiler.java" 3706]
             [clojure.lang.Compiler$InvokeExpr eval "Compiler.java" 3705]
             [clojure.lang.Compiler$InvokeExpr eval "Compiler.java" 3705]]
            :cause "BOOM-3"
            :data {:boom "3"}
            :stacktrace-type :throwable})))

  (testing "Includes a `:phase` for the causes that include it"
    (is+ [{:phase :macro-syntax-check}
          {:phase nil}]
         (catch-and-analyze (eval '(let [1])))))

  (testing "Does not include `:phase` for vanilla runtime exceptions"
    (is+ [{:phase nil}]
         (catch-and-analyze (throw (ex-info "" {}))))))

(deftest tooling-frame-name?
  (are [frame-name] (true? (#'sut/tooling-frame-name? frame-name))
    "cider.foo"
    "refactor-nrepl.middleware/wrap-refactor"
    "shadow.cljs.devtools.server.nrepl/shadow-inint"
    ;; `apply` typically is internal, should be hidden:
    "clojure.core/apply"
    "clojure.core/binding-conveyor-fn/fn"
    "clojure.core.protocols/iter-reduce"
    "clojure.core/eval"
    "clojure.core/with-bindings*"
    "clojure.lang.MultiFn/invoke"
    "clojure.lang.LazySeq/sval"
    "clojure.lang.Var/invoke"
    "clojure.lang.AFn/applyTo"
    "clojure.lang.AFn/applyToHelper"
    "clojure.lang.RestFn/invoke"
    "clojure.main/repl"
    "clojure.main$repl$read_eval_print__9234$fn__9235/invoke"
    "nrepl.foo"
    "nrepl.middleware.interruptible_eval$evaluate/invokeStatic")

  (are [frame-name] (false? (#'sut/tooling-frame-name? frame-name))
    "acider.foo"
    "anrepl.foo"
    ;; `+` is "application" level, should not be hidden:
    "clojure.core/+"
    ;; important case - `Numbers` is relevant, should not be hidden:
    "clojure.lang.Numbers/divide"
    ;; Important unless it is at the root of the stack
    "java.lang.Thread/run"))

(deftest flag-tooling
  (is (= [{:name "cider.foo", :flags #{:tooling}}
          {:name "java.lang.Thread/run"} ;; does not get the flag because it's not the root frame
          {:name "don't touch me 1"}
          {:name "nrepl.foo", :flags #{:tooling}}
          {:name "clojure.lang.RestFn/invoke", :flags #{:tooling}}
          {:name "don't touch me 2"}
          ;; gets the flag because it's the root frame:
          {:name "java.lang.Thread/run", :flags #{:tooling}}]
         (#'sut/flag-tooling [{:name "cider.foo"}
                              {:name "java.lang.Thread/run"}
                              {:name "don't touch me 1"}
                              {:name "nrepl.foo"}
                              {:name "clojure.lang.RestFn/invoke"}
                              {:name "don't touch me 2"}
                              {:name "java.lang.Thread/run"}]))
      "Adds the flag when appropiate, leaving other entries untouched")

  (let [frames [{:name "don't touch me"}
                {:name "java.util.concurrent.FutureTask/run"}
                {:name "java.util.concurrent.ThreadPoolExecutor/runWorker"}
                {:name "java.util.concurrent.ThreadPoolExecutor$Worker/run"}]]
    (is (= [{:name "don't touch me"}
            {:name "java.util.concurrent.FutureTask/run", :flags #{:tooling}}
            {:name "java.util.concurrent.ThreadPoolExecutor/runWorker", :flags #{:tooling}}
            {:name "java.util.concurrent.ThreadPoolExecutor$Worker/run", :flags #{:tooling}}]
           (#'sut/flag-tooling frames))
        "Three j.u.concurrent frames get the flag if they're at the bottom")
    (is (= [{:name "don't touch me"}
            {:name "java.util.concurrent.FutureTask/run"}
            {:name "java.util.concurrent.ThreadPoolExecutor/runWorker"}
            {:name "java.util.concurrent.ThreadPoolExecutor$Worker/run"}
            {:name "x"}]
           (#'sut/flag-tooling (conj frames {:name "x"})))
        "The j.u.concurrent frames don't get the flag if they're not at the bottom")))
