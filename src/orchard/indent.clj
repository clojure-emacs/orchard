(ns orchard.indent
  "`:style/indent` inference."
  (:require
   [clojure.set :as set]
   [clojure.string :as str])
  (:import
   (java.util List)))

(defn- index-by-name [m]
  (into {}
        (map (fn [[k v]]
               [(-> k name) [k v]]))
        m))

(def clojure-mode-indents-exact
  "`:style/indent` rules based on clojure-mode, for names that are short
  and might be a syllable/word within a larger, unrelated word/phrase.

  e.g. `are` can be part of `compare`, so `are` should only be looked for exact matches,
  not for fuzzy matches.

  Uses the modern tuple format shared with clojure-mode and clojure-ts-mode:
  [[:block N]] — N special args, then body
  [[:inner D]] — body-style at nesting depth D
  [[:inner D I]] — body-style at depth D, only at position I"
  (index-by-name '{clojure.core/->           nil
                   clojure.core/->>          nil
                   clojure.test/are          [[:block 2]]
                   clojure.core/binding      [[:block 1]]
                   clojure.core/case         [[:block 1]]
                   catch                     [[:block 2]]
                   clojure.core/comment      [[:block 0]]
                   clojure.core/cond         [[:block 0]]
                   def                       [[:inner 0]]
                   clojure.core/defn         [[:inner 0]]
                   clojure.core/delay        [[:block 0]]
                   do                        [[:block 0]]
                   clojure.core/extend       [[:block 1]]
                   clojure.spec.alpha/fdef   [[:block 1]]
                   clojure.core/fn           [[:inner 0]]
                   clojure.core/for          [[:block 1]]
                   clojure.core.async/go     [[:block 0]]
                   if                        [[:block 1]]
                   clojure.core/let          [[:block 1]]
                   clojure.core/locking      [[:block 1]]
                   clojure.core/loop         [[:block 1]]
                   clojure.core/ns           [[:block 1]]
                   clojure.core/proxy        [[:block 2] [:inner 1]]
                   clojure.core/reify        [[:inner 0] [:inner 1]]
                   clojure.test/testing      [[:block 1]]
                   this-as                   [[:block 1]]
                   clojure.core.async/thread [[:block 0]]
                   try                       [[:block 0]]
                   clojure.core/when         [[:block 1]]
                   clojure.core/while        [[:block 1]]}))

(def clojure-mode-indents-fuzzy
  "`:style/indent` rules based on clojure-mode, for names that are deemed long/unique enough.
  See `clojure-mode-indents-exact` for format documentation."
  (index-by-name '{clojure.core.async/alt!      [[:block 0]]
                   clojure.core.async/alt!!     [[:block 0]]
                   clojure.core/as->            [[:block 2]]
                   clojure.core/bound-fn        [[:inner 0]]
                   clojure.core/cond->          [[:block 1]]
                   clojure.core/cond->>         [[:block 1]]
                   clojure.core/condp           [[:block 2]]
                   clojure.core/definterface    [[:block 1] [:inner 1]]
                   clojure.core/defmethod       [[:inner 0]]
                   clojure.core/defprotocol     [[:block 1] [:inner 1]]
                   clojure.core/defrecord       [[:block 2] [:inner 1]]
                   clojure.test/deftest         [[:inner 0]]
                   clojure.core/deftype         [[:block 2] [:inner 1]]
                   clojure.core/doseq           [[:block 1]]
                   clojure.core/dotimes         [[:block 1]]
                   clojure.core/doto            [[:block 1]]
                   clojure.core/extend-protocol [[:block 1] [:inner 0]]
                   clojure.core/extend-type     [[:block 1] [:inner 0]]
                   finally                      [[:block 0]]
                   clojure.core/future          [[:block 0]]
                   clojure.core.async/go-loop   [[:block 1]]
                   clojure.core/if-let          [[:block 1]]
                   clojure.core/if-not          [[:block 1]]
                   clojure.core/if-some         [[:block 1]]
                   clojure.core/letfn           [[:block 1] [:inner 2 0]]
                   clojure.core/some->          nil
                   clojure.core/some->>         nil
                   clojure.test/use-fixtures    [[:inner 0]]
                   clojure.core/when-first      [[:block 1]]
                   clojure.core/when-let        [[:block 1]]
                   clojure.core/when-not        [[:block 1]]
                   clojure.core/when-some       [[:block 1]]}))

(defn- symbolize [x]
  (case x
    & '&
    '_))

(defn- structure= [candidate reference]
  (set/subset? (->> candidate (map (partial mapv symbolize)) set)
               (->> reference (map (partial mapv symbolize)) set)))

(def ^:private try-requiring-resolve
  (memoize (fn [x]
             (try
               (-> x namespace symbol require)
               (resolve x)
               (catch Throwable _
                 nil)))))

(defn- acceptably-analog? [candidate-arglists clojure-core-symbol]
  (or (and (symbol? clojure-core-symbol)
           (not (namespace clojure-core-symbol)))
      (let [resolved (try-requiring-resolve clojure-core-symbol)]
        (or (not resolved)
            (structure= candidate-arglists (-> resolved meta :arglists))))))

(defn- compute-style-indent [^String macro-name [^List arglist :as arglists]]
  (let [[_ [exact-clojure-core-symbol exact-indentation] :as exact-match] (find clojure-mode-indents-exact macro-name)
        [_ [fuzzy-clojure-core-symbol fuzzy-indentation] :as fuzzy-match] (or
                                                                           ;; an exact match, when available, is of course faster and more desirable:
                                                                           (find clojure-mode-indents-fuzzy macro-name)
                                                                           (->> clojure-mode-indents-fuzzy
                                                                                (some (fn [[k _v :as entry]]
                                                                                        (when (str/includes? macro-name k)
                                                                                          entry)))))
        one-arglist? (-> arglists count (= 1))
        def-like? (re-find #"^def" macro-name)
        result (cond
                 exact-match
                 (when (acceptably-analog? arglists exact-clojure-core-symbol)
                   exact-indentation)

                 fuzzy-match
                 (when (acceptably-analog? arglists fuzzy-clojure-core-symbol)
                   fuzzy-indentation)

                 ;; def-like macros get no inference - these tend to be risky to make any assumption about
                 def-like?
                 nil

                 (and one-arglist?
                      (->> arglists first (some #{'&})))
                 (let [^List arglist (first arglists)]
                   (.indexOf arglist '&))

                 (and one-arglist?
                      (some #{'then} arglist)
                      (some #{'else} arglist))
                 (let [t (.indexOf arglist 'then)
                       e (.indexOf arglist 'else)]
                   (when (and (> e t)
                              (= e (-> arglist count dec)))
                     t))

                 one-arglist?
                 (->> ['body
                       'forms
                       'clauses
                       'args
                       'body-expr
                       'fntail
                       'fn-tail]
                      (reduce (fn [_ v]
                                (let [i (.indexOf arglist v)]
                                  (when-not (= -1 i)
                                    (reduced i))))
                              nil)))]
    result))

(defn infer-style-indent
  "Given a `metadata` map obtained from a Clojure var object,
  associates a `:style/indent` value based on inference (rule of thumb) rules:

  * The macro name is inspected in conjunction with the arglist,
    in search for an acceptably analog match with a clojure.core counterpart.
  * If that fails, the position of `&` is observed.
  * Otherwise, the argument names are observed.

  The exact inference logic is an implementation detail.

  `:style/indent` will be not associated if no rule matched."
  [{:keys [arglists]
    macro-name :name
    :as metadata}]
  (let [result (compute-style-indent (str macro-name)
                                     arglists)]
    (cond-> metadata
      result (assoc :style/indent result))))
