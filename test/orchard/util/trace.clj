(ns orchard.util.trace)

;; (set! clojure.core/*print-length* 3)

(defn trace!
  "Given a function var, trace its result."
  [v]
  (let [m    (meta v)
        n    (symbol (str (ns-name (:ns m))) (str (:name m)))
        orig (::original-var m @v)]
    (alter-var-root v (constantly (fn [& args]
                                    (binding [clojure.core/*print-length* 3]
                                      (let [result (apply orig args)]
                                        (prn (cons n args))
                                        (println "=>" result)
                                        result)))))
    (alter-meta! v assoc ::original-var orig)))

(defn untrace! [v]
  (when-let [orig (::original-var (meta v))]
    (alter-var-root v (constantly orig))
    (alter-meta! v dissoc ::original-var)))
