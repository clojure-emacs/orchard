(ns orchard.cljs.test-env
  (:require
   [clojure.test :as test #?(:clj :refer :cljs :refer-macros) [deftest is testing]]
   [clojure.set :as set]
   [cljs.env :as env]
   #?@(:clj [[cljs.analyzer.api :as ana]
             [cljs.build.api :as build]
             [cljs.compiler.api :as comp]
             [clojure.java.io :as io]
             [cljs.repl]]
       :cljs [[lumo.repl :as repl]])))

(def +test-namespace+ "orchard/test_ns.cljc")
(def +test-output-dir+ "target/cljs-out")

(defn create-test-env []
  #?(:clj
     (let [opts (build/add-implicit-options {:cache-analysis true, :output-dir +test-output-dir+})
           env (env/default-compiler-env opts)]
       (comp/with-core-cljs env opts
         (fn []
           (if-let [test-resource (io/resource +test-namespace+)]
             (ana/analyze-file env test-resource opts)
             (throw (ex-info (str "Cannot find test file " +test-namespace+) {:test-namespace +test-namespace+})))))
       @env)

     :cljs
     (do (repl/eval '(require (quote orchard.cljs.test-ns)) 'orchard.cljs.test-env)
         @env/*compiler*)))

(def ^:dynamic *env*)

(defn wrap-test-env
  [f]
  (binding [*env* (create-test-env)]
    (f)))
