(ns bau.core
  (:gen-class)
  (:require [cli-matic.core :refer [run-cmd]]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [datoteka.core :as fs]
            [sci.core :as sci]))

(def workspace
  (fs/path [fs/*cwd* "examples"]))

(def workspace-file
  (some
    #(when (-> % (fs/name) (= "WORKSPACE.bau")) %)
    (fs/list-dir workspace)))

(def build-files
  (filter
    #(when (-> % (fs/name) (= "BUILD.bau")) %)
    (fs/list-dir workspace)))

(defn substitute
  [template & substitutions]
  (let [substitutions
        (apply hash-map substitutions)]
    (str/replace
      template
      #"\{([a-z-]+)\}"
      (fn [[_ match]]
        (str (get substitutions (keyword match)))))))

(def defrule ^:sci/macro
  (fn [_ _ type argv body]
    `(defmethod ~'eval-rule ~type
       ~(into ['_] argv)
       ~body)))

(def deftarget ^:sci/macro
  (fn [_ _ name type & args]
    (let [args (apply hash-map args)]
      `(~'eval-rule ~type ~'ctx ~args))))

(defprotocol Action
  (run-shell [mode {:keys [outputs cmd]}]))

(defrecord LogActions []
  Action
  (run-shell [mode args]
    (println [:actions/run-shell mode args])))

(defn init-env []
  (let [env (atom {})]
    (sci/eval-string
      (pr-str
        '(do
           (defmulti eval-rule (fn [type ctx args] type))))
      {:env env})
    env))

(defn load-rules [env]
  (sci/eval-string
   (slurp workspace-file)
   {:env env
    :bindings {'defrule defrule}
    :namespaces {'actions {'run-shell run-shell}}}))

(defn load-targets [env]
  (doseq [file build-files]
   (-> file
     slurp
     (sci/eval-string
       {:env env
        :bindings {'deftarget deftarget
                   'ctx (->LogActions)}}))))

(->> @(doto (init-env)
        (load-rules)
        (load-targets))
  :namespaces
  (remove
    (fn [[ns _]] (str/starts-with? (name ns) "clojure")))
  (into {}))
