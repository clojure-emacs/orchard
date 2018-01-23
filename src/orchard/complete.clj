(ns orchard.complete
  (:require [cljs-tooling.complete :as cljs-complete]
            [compliment.core :as jvm-complete]
            [compliment.utils :as jvm-complete-utils]
            [orchard.misc :as u]
            [orchard.util.cljs :as cljs]))

(defn complete
  [{:keys [ns symbol context extra-metadata] :as msg}]
  (let [ns (u/as-sym ns)
        prefix (str symbol)
        extra-metadata (set (map keyword extra-metadata))]
    (if-let [cljs-env (cljs/grab-cljs-env msg)]
      (cljs-complete/completions cljs-env prefix {:context-ns ns
                                                  :extra-metadata extra-metadata})
      (jvm-complete/completions prefix {:ns ns
                                        :context context
                                        :extra-metadata extra-metadata}))))

(defn complete-doc
  [{:keys [ns symbol] :as msg}]
  (when-not (cljs/grab-cljs-env msg)
    (jvm-complete/documentation (str symbol) (u/as-sym ns))))

(def flush-caches jvm-complete-utils/flush-caches)
