(ns bau.core
  (:gen-class)
  (:require [cli-matic.core :refer [run-cmd]]
            [clojure.string :as str]
            [datoteka.core :as fs]
            [sci.core :as sci]))

(def deftarget-list ^:sci/macro
  (fn [_&form _&env name type & args]
    (let [spec
          (merge
            (apply array-map args)
            `#:target{:name '~name
                      :type ~type})]
      spec)))

(defn list-file
  [{:keys [root]} file]
  (let []
    (map
      (fn [form]
        (-> form
          (sci/eval-string {:bindings   {'deftarget  deftarget-list}})
          (update :target/name #(str "//" (fs/relativize (fs/parent file) root) ":" (str %)))))
      (str/split (slurp file) #"(?m)^$"))))

(defn find-workspace-root
  [dir]
  (some
    (fn [dir] (when (seq (fs/list-dir dir "WORKSPACE.bau")) dir))
    (take-while some? (iterate fs/parent dir))))

(defn query-targets
  [{:keys [prefix]}]
  (let [root (find-workspace-root fs/*cwd*)
        targets (->> (fs/file root)
                  (file-seq)
                  (map fs/path)
                  (filter #(-> % (fs/name) (= "BUILD.bau")))
                  (mapcat (partial list-file {:root root}))
                  (map :target/name)
                  (filter #(if prefix (str/starts-with? % prefix) true)))]
    (run! println targets)))


(def options
  {:command     "bau"
   :description "An incremental build system"
   :version     "0.0"
   :subcommands
   [{:command     "query"
     :description "Lists all targets matching the path prefix, or the current directory if no prefix is given"
     :opts        [{:option  "prefix"
                    :short   0
                    :as      "Path prefix"
                    :type    :string
                    :default nil}]
     :runs        query-targets}]})


(defn -main
  [& args]
  (run-cmd args options))
