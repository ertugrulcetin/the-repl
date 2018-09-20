(ns the-repl.deps
  (:require [cemerick.pomegranate :as pom]
            [the-repl.namespace :as namesp]
            [clojure.tools.namespace.find :as ff])
  (:import (java.util.jar JarFile)))


(def repo {"central" "https://repo1.maven.org/maven2/"
           "clojars" "https://clojars.org/repo"})

(def sep (System/getProperty "file.separator"))

(def home-dir (System/getProperty "user.home"))


(defn- artifact->path
  [[name version] sep]
  (let [split-name-vec (clojure.string/split (str name) #"/")
        split-name-vec (if (= (count split-name-vec) 1) (apply concat (repeat 2 split-name-vec)) split-name-vec)
        split-name-vec (interpose sep split-name-vec)]
    (apply str (concat split-name-vec [sep version sep]))))


(defn- get-jar-name
  [[name version]]
  (let [name (clojure.string/split (str name) #"/")
        name (last name)]
    (str name "-" version ".jar")))


(defn- get-jar-path
  [dep]
  (str home-dir
       sep
       ".m2"
       sep
       "repository"
       sep
       (artifact->path dep sep)
       (get-jar-name dep)))


(defn- get-ns-decls
  [dep]
  (ff/find-ns-decls-in-jarfile (JarFile. ^String (get-jar-path dep))))


(defn- require-nses
  [ns-decls]
  (doseq [ns* (namesp/get-topo-sorted-nses ns-decls)]
    (try
      (require ns*)
      (catch Exception e
        (println "Ns could not loaded: " ns*)))))


(defn install-deps
  [deps]
  (doseq [dep deps]
    (try
      (pom/add-dependencies :coordinates [dep] :repositories repo)
      (require-nses (get-ns-decls dep))
      (catch Exception e
        (println e)
        ;;TODO log exception
        ))))


(comment
  (install-deps '[[compojure "1.6.1"]])
  (ns-publics 'compojure.middleware)
  )


