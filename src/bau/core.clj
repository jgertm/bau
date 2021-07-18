(ns bau.core
  (:gen-class)
  (:require [cli-matic.core :refer [run-cmd]]
            [clojure.java.shell :as shell]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [datoteka.core :as fs] ;; TODO: replace, not very good
            loom.alg
            loom.graph
            loom.label
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
       ~(into ['_type] argv)
       ~body)))

(def graph (atom nil))

(def deftarget ^:sci/macro
  (fn [_ _ target type & args]
    (let [args (apply hash-map args)
          add-node
          (fn add-node [target action]
            (swap! graph #(-> %
                            (loom.graph/add-edges [target action])
                            (loom.label/add-label target {:action/type :return
                                                          :inputs      [action]}))))]
      `(let [action (~'eval-rule ~type ~args)]
         (~add-node (quote ~target) action)
         (def ~target (quote ~target))))))

(defn analyze-action [type {:keys [inputs] :as args}]
  (let [node (keyword (gensym (name type)))]
    (swap! graph
      (fn [g]
        (reduce
          (fn [g n] (loom.graph/add-edges g (vector node n)))
          (-> g
            (loom.graph/add-nodes node)
            (loom.label/add-label node (merge {:action/type type} args)))
          inputs)))
    node))

(defn init-env []
  (let [env (atom {})]
    (sci/eval-string
      (pr-str
        '(do
           (defmulti eval-rule (fn [type args] type))))
      {:env env})
    env))

(defn load-rules [env]
  (sci/eval-string
    (slurp workspace-file)
    {:env        env
     :bindings   {'defrule defrule}
     :namespaces {'actions {'run-shell! (partial analyze-action :run-shell!)}}})
  env)

(defn load-targets [env]
  (reset! graph (loom.graph/digraph))
  (doseq [file build-files]
    (-> file
      slurp
      (sci/eval-string
        {:env      env
         :bindings {'deftarget deftarget}})))
  graph)

(defn load-graph []
  (-> (init-env)
    (load-rules)
    (load-targets)
    (deref)))

(defn provision-sandbox
  [{:keys [inputs]}]
  (let [dir (fs/create-tempdir "bau")]
    (doseq [input inputs]
      ;; TODO: copy/link files
      (java.nio.file.Files/copy
        input
        (fs/join dir (fs/name input))
        (make-array java.nio.file.CopyOption 0))
      ;; TODO: permissions etc.
      nil)
    {:dir dir}))

(defn exec-task
  [spec]
  (case (:action/type spec)
    :run-shell!
    (let [{:keys [dir]} (provision-sandbox spec)
          {:keys [cmd outputs]} spec
          {:keys [exit] :as proc}
          (shell/sh "sh" "-c" (str/join " " cmd) :dir dir)]
      #_(pprint/pprint spec)
      (if-not (zero? exit)
        (throw (ex-info "Subprocess failed" proc))
        (for [output outputs
              :let   [output (fs/path dir output)]]
          (if (fs/exists? output)
            output
            (throw (ex-info "Didn't create specified output" {:output output}))))))

    :return
    (do
      #_(pprint/pprint spec)
      (:inputs spec))))

(defn build-target
  [graph target]
  (deref (get
           (->> target
             (loom.alg/topsort graph)
             reverse
             (reduce
               (fn [acc target]
                 #_(pprint/pprint {:acc acc})
                 (let [{:keys [inputs] :as spec} (loom.label/label graph target)]
                   (assoc acc target
                     (future
                       (let [inputs (mapcat #(deref (get acc %)) inputs)]
                         #_(println (format "> starting %s" target))
                         (-> spec
                           (assoc :inputs inputs)
                           exec-task)
                         #_(println (format "> finished %s" target)))))))
               {}))
           target)))

(println "\n====")
(build-target (load-graph) 'another)
