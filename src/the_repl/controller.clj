(ns the-repl.controller
  (:require [seesaw.core :refer :all]
            [seesaw.chooser :as chooser]
            [seesaw.dev :as dev]
            [the-repl.util :as util]
            [the-repl.view :as view]
            [the-repl.core :as core]
            [the-repl.brackets :as brackets]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.async :refer [chan sliding-buffer go-loop <! timeout put!]]
            [kezban.core :refer :all])
  (:import (java.awt Color)
           (javax.swing.text StyleConstants
                             SimpleAttributeSet
                             DefaultHighlighter$DefaultHighlightPainter)
           (javax.swing.event DocumentListener)))


(defn- append-to-repl
  [s]
  (let [pane (util/get-widget-by-id :repl-text-area)
        doc  (.getDocument pane)]
    (.insertString doc (.getLength doc) s nil)))


(defn- run-code-in-repl
  [code]
  (append-to-repl (str code "\n"))
  (let [results (core/eval-code code)]
    (doseq [r results]
      (cond-let [value (:value r)
                 ex (:ex r)
                 err (:err r)
                 out (:out r)]

                value
                (append-to-repl (str "=> " value "\n"))

                out
                (append-to-repl out)

                ex
                (append-to-repl (str ex "\n"))

                err
                (append-to-repl (str err "\n\n"))))
    (scroll! (util/get-widget-by-id :repl-text-area) :to :bottom)))


(defn- add-open-file-event
  []
  (listen (util/get-widget-by-id :open-file-button)
          :mouse-clicked (fn [_]
                           (chooser/choose-file :selection-mode :files-only
                                                :success-fn (fn [fc file]
                                                              (text! (util/get-widget-by-id :editor-text-area)
                                                                     (str (slurp file) "\n")))))))

(defn- add-clean-editor-event
  []
  (listen (util/get-widget-by-id :clear-button)
          :mouse-clicked (fn [_]
                           (text! (util/get-widget-by-id :editor-text-area)
                                  "(ns the-repl)\n\n")
                           (.requestFocus (util/get-widget-by-id :editor-text-area)))))


(defn- add-save-file-event
  []
  (listen (util/get-widget-by-id :save-file-button)
          :mouse-clicked (fn [_]
                           (chooser/choose-file :type :save
                                                :success-fn (fn [fc file]
                                                              (let [path (.getAbsolutePath file)
                                                                    path (if (str/ends-with? path ".clj") path (str path ".clj"))
                                                                    file (io/file path)]
                                                                (spit file (value (util/get-widget-by-id :editor-text-area)))))))))


(defn- add-run-code-event
  []
  (listen (util/get-widget-by-id :run-code-button)
          :mouse-clicked (fn [_]
                           (run-code-in-repl (value (util/get-widget-by-id :editor-text-area))))))


(defn- add-start-repl-event
  []
  (listen (util/get-widget-by-id :repl-start-button)
          :mouse-clicked (fn [_]
                           (append-to-repl "\nConnecting to local nREPL server...\n")
                           (core/start-server)
                           (append-to-repl (format "nREPL server started on port %d on host 127.0.0.1 - nrepl://127.0.0.1:%d\n"
                                                   (core/get-server-port)
                                                   (core/get-server-port))))))


(defn- add-clean-repl-editor-event
  []
  (listen (util/get-widget-by-id :repl-clear-button)
          :mouse-clicked (fn [_]
                           (text! (util/get-widget-by-id :repl-text-area) ""))))


(defn- add-stop-repl-event
  []
  (listen (util/get-widget-by-id :repl-stop-button)
          :mouse-clicked (fn [_]
                           (core/stop-server)
                           (append-to-repl "REPL Stopped."))))



(defn render-highlights
  [editor sas sd]
  (invoke-later
    (let [code       (value editor)
          code-count (count code)
          _          (brackets/generate-indices! code)]
      (do
        (StyleConstants/setForeground sas Color/BLACK)
        (.setCharacterAttributes sd 0 code-count sas true)


        (StyleConstants/setForeground sas (Color/decode "#0000FF"))
        (doseq [[_ idx] (:number-indices @brackets/indices-map)]
          (.setCharacterAttributes sd idx 1 sas true))

        (StyleConstants/setForeground sas (Color/decode "#000080"))
        (StyleConstants/setBold sas true)
        (doseq [[start-i end-i] (:fn-hi-indices @brackets/indices-map)]
          (.setCharacterAttributes sd start-i (- end-i start-i) sas true))

        (StyleConstants/setForeground sas (Color/decode "#007F00"))
        (StyleConstants/setBold sas false)
        (doseq [[_ idx v] (:char-indices @brackets/indices-map)]
          (.setCharacterAttributes sd idx v sas true))

        (StyleConstants/setForeground sas (Color/decode "#660E7A"))
        (StyleConstants/setItalic sas true)
        (doseq [[start-i end-i] (:keyword-indices @brackets/indices-map)]
          (.setCharacterAttributes sd start-i (- end-i start-i) sas true))

        (StyleConstants/setForeground sas (Color/decode "#808080"))
        (StyleConstants/setItalic sas true)
        (doseq [[start-i end-i] (:comment-quote-indices @brackets/indices-map)]
          (.setCharacterAttributes sd start-i (- end-i start-i) sas true))

        (StyleConstants/setForeground sas (Color/decode "#007F00"))
        (StyleConstants/setItalic sas false)
        (doseq [[start-i end-i] (:double-quote-indices @brackets/indices-map)]
          (.setCharacterAttributes sd start-i (- end-i (dec start-i)) sas true)))
     )))


(def ll (let [editor   (util/get-widget-by-id :editor-text-area)
              painter  (DefaultHighlighter$DefaultHighlightPainter. (Color/decode "#b4d5fe"))
              sas      (SimpleAttributeSet.)
              sd       (.getStyledDocument editor)
              document (.getDocument editor)
              _        (.addDocumentListener document (proxy [DocumentListener] []
                                                        (removeUpdate [e]
                                                          (render-highlights editor sas sd))
                                                        (insertUpdate [e]
                                                          (render-highlights editor sas sd))
                                                        (changedUpdate [e])))]
          (listen (util/get-widget-by-id :editor-text-area)
                  :caret-update (fn [_]
                                  (invoke-later
                                    (let [caret-idx           (.getCaretPosition editor)
                                          hi                  (.getHighlighter editor)
                                          all-his             (.getHighlights hi)
                                          closing-bracket-idx (get-in @brackets/indices-map [:match-brackets-indices :open caret-idx])
                                          opening-bracket-idx (get-in @brackets/indices-map [:match-brackets-indices :close (dec caret-idx)])]
                                      (doseq [h all-his]
                                        (.removeHighlight hi h))
                                      (when closing-bracket-idx
                                        (.addHighlight hi caret-idx (inc caret-idx) painter)
                                        (.addHighlight hi closing-bracket-idx (inc closing-bracket-idx) painter))
                                      (when opening-bracket-idx
                                        (.addHighlight hi (dec caret-idx) caret-idx painter)
                                        (.addHighlight hi opening-bracket-idx (inc opening-bracket-idx) painter))))))))


(comment
  (ll))

(defn register-editor-events
  []
  (add-open-file-event)
  (add-save-file-event)
  (add-clean-editor-event)
  (add-run-code-event)
  (add-start-repl-event)
  (add-clean-repl-editor-event)
  (add-stop-repl-event))

(comment
  (register-editor-events))