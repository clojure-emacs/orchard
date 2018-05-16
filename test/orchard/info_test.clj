(ns orchard.info-test
  (:require [clojure.test :refer :all]
            [clojure.repl :as repl]
            [orchard.info :as info]))

(deftest see-also-test
  (is (not-empty (info/see-also 'clojure.core 'map))))

(deftype T [])

(deftest info-test
  (is (info/info 'orchard.info 'io))

  (is (info/info 'orchard.info 'info))

  (is (info/info 'orchard.info 'java.lang.Class))
  (is (info/info 'orchard.info 'Class/forName))
  (is (info/info 'orchard.info '.toString))

  (is (not (info/info 'clojure.core (gensym "non-existing"))))
  (is (info/info 'orchard.info-test 'T)
      "Check that deftype T (which returns nil for .getPackage), doesn't throw")

  (is (= (the-ns 'clojure.core) (:ns (info/info 'orchard.info 'str))))

  ;; special forms are marked as such and nothing else is (for all syms in ns)
  (let [ns 'orchard.info
        spec-forms (into '#{letfn fn let loop} (keys @#'repl/special-doc-map))
        infos (->> (into spec-forms (keys (ns-map ns)))
                   (map (partial info/info ns)))]
    (is (= spec-forms (->> (-> (group-by :special-form infos)
                               (get true))
                           (map :name)
                           (set))))))

(deftest info-java-test
  (is (info/info-java 'clojure.lang.Atom 'swap)))
