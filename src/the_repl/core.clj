(ns the-repl.core
  (:require [nrepl.server :as nserver]
            [nrepl.core :as repl])
  (:import (java.net ServerSocket)))


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



(comment
  (start-server)

  (with-open [conn (repl/connect :port (get-server-port))]
    (-> (repl/client conn 1000)
        (repl/message {:op :eval :code "(map inc [1 2 3]) 4 (defn aa\n  []\n  ) (1 2)"})
        clojure.pprint/pprint))
  )
