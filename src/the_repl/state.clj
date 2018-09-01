(ns the-repl.state)


(defonce states (atom {}))


(defn repl-server-on?
  []
  (:repl-server-on? @states))


(defn set-repl-server!
  [x]
  (swap! states assoc :repl-server-on? x))


(defn set-opened-from-file!
  [x]
  (swap! states assoc :opened-from-file? x))


(defn set-file-saved!
  [x]
  (swap! states assoc :file-saved? x))