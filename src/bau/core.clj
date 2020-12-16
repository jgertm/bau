(ns bau.core
  (:require [datoteka.core :as fs]
            [sci.core :as sci]
            [clojure.string :as str]))

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
        (->> substitutions
          (apply hash-map)
          (map (fn [[k v]] [k (str v)]))
          (into {}))]
    (str/replace
      template
      #"\{([a-z-]+)\}"
      (fn [[_ match]]
        (get substitutions (keyword match))))))

(comment
  (substitute "{foo}-{bar}" :foo 1 :bar 2)

  )


(def defrule ^:sci/macro
  (fn [_ _ type argv body]
    `(defmethod ~'eval-rule ~type
       ~argv
       ~body)))

(def deftarget ^:sci/macro
  (fn [_&form _&env name type & args]
    (let [spec
          (merge
            (apply array-map args)
            `#:target{:name '~name
                      :type ~type})]
      `(vector (~'eval-rule nil ~spec)
           ~spec))))

(defn read-file
  [file]
  (let [env     (atom {})
        actions (atom [])
        forms
        (-> file
          (slurp)
          (str/split #"(?m)^$"))]
    (sci/eval-string "(defmulti eval-rule (fn [_ {:target/keys [type]}] type))" {:env env})
    (run!
      #(sci/eval-string %
         {:env        env
          :bindings   {'deftarget  deftarget
                       'defrule    defrule
                       'substitute substitute}
          :namespaces {'actions {'declare-file
                                 (fn [name]
                                   (let [path (fs/path name)]
                                     (swap! actions conj [:actions/declare-file path])
                                     path))
                                 'run-shell
                                 (fn [cmd & args]
                                   (swap! actions conj [:actions/run-shell cmd args])
                                   nil)}}})
      forms)
    @actions))

(comment 

  (read-file (first build-files))

  (read-file workspace-file)

  )
