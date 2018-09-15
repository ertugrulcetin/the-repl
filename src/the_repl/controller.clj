(ns the-repl.controller
  (:require [seesaw.core :refer :all]
            [seesaw.chooser :as chooser]
            [the-repl.util :as util]
            [the-repl.core :as core]
            [the-repl.brackets :as brackets]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [kezban.core :refer :all])
  (:import (java.awt Color)
           (javax.swing.text StyleConstants
                             SimpleAttributeSet
                             DefaultHighlighter$DefaultHighlightPainter)
           (javax.swing.event DocumentListener)))


(def color-map {:normal   Color/BLACK
                :number   (Color/decode "#0000FF")
                :fn       (Color/decode "#000080")
                :bool     (Color/decode "#000080")
                :char     (Color/decode "#007F00")
                :keyword  (Color/decode "#660E7A")
                :comment  (Color/decode "#808080")
                :double-q (Color/decode "#007F00")})


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


(defmulti highlight-chars! #(-> % :type))


(defmethod highlight-chars! :reset
  [{:keys [sas sd editor] :or {sas (SimpleAttributeSet.)}}]
  (let [caret-idx (.getCaretPosition editor)]
    (StyleConstants/setForeground sas (:normal color-map))
    (.setCharacterAttributes sd caret-idx (- (count (:all-chars-indices @brackets/indices-map)) caret-idx) sas true)))


(defmethod highlight-chars! :numbers
  [{:keys [sas sd] :or {sas (SimpleAttributeSet.)}}]
  (StyleConstants/setForeground sas (:number color-map))
  (doseq [[_ idx] (:number-indices @brackets/indices-map)]
    (.setCharacterAttributes sd idx 1 sas true)))


(defmethod highlight-chars! :fns
  [{:keys [sas sd] :or {sas (SimpleAttributeSet.)}}]
  (StyleConstants/setForeground sas (:fn color-map))
  (StyleConstants/setBold sas true)
  (doseq [[start-i end-i] (:fn-hi-indices @brackets/indices-map)]
    (.setCharacterAttributes sd start-i (- end-i start-i) sas true)))


(defmethod highlight-chars! :bools
  [{:keys [sas sd] :or {sas (SimpleAttributeSet.)}}]
  (StyleConstants/setForeground sas (:bool color-map))
  (StyleConstants/setBold sas true)
  (doseq [i (:true-idxs (:true-false-indices @brackets/indices-map))]
    (.setCharacterAttributes sd i 4 sas true))
  (doseq [i (:false-idxs (:true-false-indices @brackets/indices-map))]
    (.setCharacterAttributes sd i 5 sas true)))


(defmethod highlight-chars! :chars
  [{:keys [sas sd] :or {sas (SimpleAttributeSet.)}}]
  (StyleConstants/setForeground sas (:char color-map))
  (StyleConstants/setBold sas false)
  (doseq [[_ idx v] (:char-indices @brackets/indices-map)]
    (.setCharacterAttributes sd idx v sas true)))


(defmethod highlight-chars! :keywords
  [{:keys [sas sd] :or {sas (SimpleAttributeSet.)}}]
  (StyleConstants/setForeground sas (:keyword color-map))
  (StyleConstants/setItalic sas true)
  (doseq [[start-i end-i] (:keyword-indices @brackets/indices-map)]
    (.setCharacterAttributes sd start-i (- end-i start-i) sas true)))


(defmethod highlight-chars! :comments
  [{:keys [sas sd] :or {sas (SimpleAttributeSet.)}}]
  (StyleConstants/setForeground sas (:comment color-map))
  (StyleConstants/setItalic sas true)
  (doseq [[start-i end-i] (:comment-quote-indices @brackets/indices-map)]
    (.setCharacterAttributes sd start-i (- end-i start-i) sas true)))


(defmethod highlight-chars! :double-quote
  [{:keys [sas sd] :or {sas (SimpleAttributeSet.)}}]
  (StyleConstants/setForeground sas (:double-q color-map))
  (StyleConstants/setItalic sas false)
  (doseq [[start-i end-i] (:double-quote-indices @brackets/indices-map)]
    (.setCharacterAttributes sd start-i (- end-i (dec start-i)) sas true)))


(defn render-highlights!
  [editor sd]
  (invoke-later
    (let [code (value editor)
          _    (brackets/generate-indices! code)
          m    {:sd sd :editor editor}]
      (highlight-chars! (merge m {:type :reset}))
      (highlight-chars! (merge m {:type :numbers}))
      (highlight-chars! (merge m {:type :fns}))
      (highlight-chars! (merge m {:type :bools}))
      (highlight-chars! (merge m {:type :chars}))
      (highlight-chars! (merge m {:type :keywords}))
      (highlight-chars! (merge m {:type :comments}))
      (highlight-chars! (merge m {:type :double-quote})))))


(def ll (let [editor         (util/get-widget-by-id :editor-text-area)
              painter        (DefaultHighlighter$DefaultHighlightPainter. (Color/decode "#b4d5fe"))
              sd             (.getStyledDocument editor)
              document       (.getDocument editor)

              _              (.addDocumentListener document (proxy [DocumentListener] []
                                                              (removeUpdate [e]
                                                                (println "Remove")
                                                                (render-highlights! editor sd))
                                                              (insertUpdate [e]
                                                                (println "Insert")
                                                                (render-highlights! editor sd))
                                                              (changedUpdate [e])))
              pre-highlights (atom [])]
          (listen editor
                  :caret-update (fn [_]
                                  (invoke-later
                                    (let [caret-idx           (.getCaretPosition editor)
                                          hi                  (.getHighlighter editor)
                                          closing-bracket-idx (get-in @brackets/indices-map [:match-brackets-indices :open caret-idx])
                                          opening-bracket-idx (get-in @brackets/indices-map [:match-brackets-indices :close (dec caret-idx)])]
                                      (doseq [h @pre-highlights]
                                        (.removeHighlight hi h))
                                      (when closing-bracket-idx
                                        (reset! pre-highlights [(.addHighlight hi caret-idx (inc caret-idx) painter)
                                                                (.addHighlight hi closing-bracket-idx (inc closing-bracket-idx) painter)]))
                                      (when opening-bracket-idx
                                        (reset! pre-highlights [(.addHighlight hi (dec caret-idx) caret-idx painter)
                                                                (.addHighlight hi opening-bracket-idx (inc opening-bracket-idx) painter)]))))))))

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