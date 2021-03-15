(ns orchard.eldoc
  "Some helper functions to support eldoc-like functionality
  in editors."
  {:author "Bozhidar Batsov"})

(defn- extract-arglists
  [info]
  (cond
    (:special-form info) (->> (:forms info)
                              ;; :forms contains a vector of sequences or symbols
                              ;; which we have to convert the format employed by :arglists
                              (map #(if (coll? %) (vec %) (vector %))))
    (:candidates info) (->> (:candidates info)
                            vals
                            (mapcat :arglists)
                            distinct
                            (sort-by count))
    :else (:arglists info)))

(defn- format-arglists [raw-arglists]
  (map #(mapv str %) raw-arglists))

(defn- extract-ns-or-class
  [{:keys [ns class candidates]}]
  (cond
    ns {:ns (str ns)}
    class {:class [(str class)]}
    candidates {:class (map key candidates)}))

(defn- extract-name-or-member
  [{:keys [name member candidates]}]
  (cond
    name {:name (str name)}
    member {:member (str member)}
    candidates {:member (->> candidates vals (map :member) first str)}))

(defn- extract-eldoc
  [info]
  (if-let [arglists (seq (-> info extract-arglists format-arglists))]
    {:eldoc arglists :type "function"}
    {:type "variable"}))

(defn eldoc
  "Generate an eldoc from `info`.

  The result is a map featuring:

  * :ns or :class - depending on whether we're dealing with Clojure or Java symbol
  * :name or :member - depending on whether we're dealing with Clojure or Java symbol
  * :eldoc - a list of type signatures suitable to be displayed by Emacs's eldoc
  * :type - the type of the symbol (e.g. function, variable, special-form)
  * :docstring

  See `orchard.info/info` for the structure of the `info`."
  [info]
  (merge (extract-ns-or-class info)
         (extract-name-or-member info)
         (extract-eldoc info)
         {:docstring (:doc info)}))

(defn datomic-query
  "Generate an eldoc string for a datomic query."
  [ns sym]
  (let [ns (read-string ns)
        sym (read-string sym)
        query (if (symbol? sym)
                (deref (ns-resolve ns sym))
                (eval sym))
        inputs (if (map? query)
                 ;; query as map
                 (or (:in query) "$")
                 ;; query as vector
                 (let [partitioned (partition-by keyword? query)
                       index (.indexOf ^clojure.lang.LazySeq partitioned '(:in))]
                   (if (= index -1)
                     "$"
                     (nth partitioned (+ 1 index)))))]
    {:inputs (format-arglists [inputs])}))
