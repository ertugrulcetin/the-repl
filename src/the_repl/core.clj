(ns the-repl.core
  (:require [nrepl.server :as nserver]
            [nrepl.core :as repl]
            [kezban.core :refer :all])
  (:import (java.net ServerSocket)
           (java.io StringWriter)))


(def server (atom nil))


(defn- get-available-port
  []
  (let [socket (ServerSocket. 0)
        port   (.getLocalPort socket)
        _      (.close socket)]
    port))


(defn get-server-port
  []
  (:port @server))


(defn start-server
  []
  (reset! server (nserver/start-server :port (get-available-port))))


(defn stop-server
  []
  (nserver/stop-server @server)
  (reset! server nil))


(defn eval-code
  [code]
  (let [s# (StringWriter.)]
    (binding [*out* s#]
      (with-open [conn (repl/connect :port (get-server-port))]
        (-> (repl/client conn Integer/MAX_VALUE)
            (repl/message {:op :eval :code code})
            clojure.pprint/pprint))
      (read-string (str s#)))))


(require '[cemerick.pomegranate :as pom])
(pom/add-dependencies :coordinates '[[medley "1.0.0"]]
                      :repositories (merge cemerick.pomegranate.aether/maven-central
                                           {"clojars" "https://clojars.org/repo"}))

(def dep '[medley "1.0.0"])

(defn artifact->path
  [[name version] sep]
  (let [split-name-vec (clojure.string/split (str name) #"/")
        split-name-vec (if (= (count split-name-vec) 1) (apply concat (repeat 2 split-name-vec)) split-name-vec)
        split-name-vec (interpose sep split-name-vec)]
    (apply str (concat split-name-vec [sep version sep]))))

(defn get-jar-name
  [[name version]]
  (str name "-" version ".jar"))

(artifact->path '[medley "1.0.0"] "/")

(def sep (System/getProperty "file.separator"))

(def jar-path (str (System/getProperty "user.home") sep ".m2" sep "repository" sep (artifact->path dep sep) (get-jar-name dep)))

(require '[clojure.tools.namespace.find :as ff])
(require (nnth (ff/find-ns-decls-in-jarfile (java.util.jar.JarFile. jar-path)) 0 1))
(def mm (ns-publics 'medley.core))

(clojure.repl/source-fn 'medley.core/filter-kv)
(slurp (:file (meta (get mm 'filter-kv))))

(comment
  (start-server)
  (eval-code "(println \"aa\")"))
