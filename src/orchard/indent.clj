(ns orchard.indent
  "`:style/indent` inference."
  (:require
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.walk :as walk])
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
  not for fuzzy matches."
  (index-by-name '{clojure.test/are          2
                   clojure.core/binding      1
                   clojure.core/case         1
                   catch                     2
                   clojure.core/comment      0
                   clojure.core/cond         0
                   def                       :defn
                   clojure.core/defn         :defn
                   clojure.core/delay        0
                   do                        0
                   clojure.core/extend       1
                   clojure.spec.alpha/fdef   1
                   clojure.core/fn           :defn
                   clojure.core/for          1
                   clojure.core.async/go     0
                   if                        1
                   clojure.core/let          1
                   clojure.core/locking      1
                   clojure.core/loop         1
                   clojure.core/ns           1
                   clojure.core/proxy        [2 nil nil [:defn]]
                   clojure.core/reify        [:defn [1]]
                   clojure.test/testing      1
                   this-as                   1
                   clojure.core.async/thread 0
                   try                       0
                   clojure.core/when         1
                   clojure.core/while        1}))

(def clojure-mode-indents-fuzzy
  "`:style/indent` rules based on clojure-mode, for names that are deemed long/unique enough."
  (index-by-name '{clojure.core.async/alt!      0
                   clojure.core.async/alt!!     0
                   clojure.core/as->            2
                   clojure.core/bound-fn        :defn
                   clojure.core/cond->          1
                   clojure.core/cond->>         1
                   clojure.core/condp           2
                   clojure.core/definterface    [1 [:defn]]
                   clojure.core/defmethod       :defn
                   clojure.core/defprotocol     [1 [:defn]]
                   clojure.core/defrecord       [2 nil nil [:defn]]
                   clojure.test/deftest         :defn
                   clojure.core/deftype         [2 nil nil [:defn]]
                   clojure.core/doseq           1
                   clojure.core/dotimes         1
                   clojure.core/doto            1
                   clojure.core/extend-protocol [1 :defn]
                   clojure.core/extend-type     [1 :defn]
                   finally                      0
                   clojure.core/future          0
                   clojure.core.async/go-loop   1
                   clojure.core/if-let          1
                   clojure.core/if-not          1
                   clojure.core/if-some         1
                   clojure.core/letfn           [1 [[:defn]] nil]
                   clojure.test/use-fixtures    :defn
                   clojure.core/when-first      1
                   clojure.core/when-let        1
                   clojure.core/when-not        1
                   clojure.core/when-some       1}))

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
               (catch Throwable e
                 nil)))))

(defn- acceptably-analog? [candidate-arglists clojure-core-symbol]
  (or (simple-symbol? clojure-core-symbol)
      (let [resolved (try-requiring-resolve clojure-core-symbol)]
        (or (not resolved)
            (structure= candidate-arglists (-> resolved meta :arglists))))))

(defn- compute-style-indent [^String macro-name [^List arglist :as arglists]]
  (let [[exact-clojure-core-symbol exact-indentation :as exact-match] (get clojure-mode-indents-exact macro-name)
        [fuzzy-clojure-core-symbol fuzzy-indentation :as fuzzy-match] (or
                                                                       ;; an exact match, when available, is of course faster and more desirable:
                                                                       (get clojure-mode-indents-fuzzy macro-name)
                                                                       (->> clojure-mode-indents-fuzzy
                                                                            (some (fn [[k v]]
                                                                                    (when (string/includes? macro-name k)
                                                                                      v)))))
        one-arglist? (-> arglists count (= 1))
        result (cond
                 exact-match
                 (when (acceptably-analog? arglists exact-clojure-core-symbol)
                   exact-indentation)

                 fuzzy-match
                 (when (acceptably-analog? arglists fuzzy-clojure-core-symbol)
                   fuzzy-indentation)

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

  * The macro name is inspected in conjuction with the arglist,
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
