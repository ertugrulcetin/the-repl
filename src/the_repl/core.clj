(ns the-repl.core
  (:require [nrepl.server :as nserver]
            [nrepl.core :as repl])
  (:import (java.net ServerSocket)
           (java.io StringWriter)))


(def server (atom nil))


(defn- get-available-port
  []
  (let [socket (ServerSocket. 0)
        port   (.getLocalPort socket)
        _      (.close socket)]
    port))


(defn- get-server-port
  []
  (:port @server))


(defn start-server
  []
  (reset! server (nserver/start-server :port (get-available-port))))


(defn stop-server
  []
  (nserver/stop-server server)
  (reset! server nil))


(defn eval-code
  [code]
  (let [s# (StringWriter.)]
    (binding [*out* s#]
      (with-open [conn (repl/connect :port (get-server-port))]
        (-> (repl/client conn 1000)
            (repl/message {:op :eval :code code})
            clojure.pprint/pprint))
      (read-string (str s#)))))


(comment
  (start-server)
  (eval-code "(println \"aa\")"))
