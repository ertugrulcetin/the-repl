(ns the-repl.view
  (:require [seesaw.core :refer :all]
            [seesaw.options :refer [apply-options]]
            [seesaw.dev :as dev]
            [seesaw.icon :as icon]
            [seesaw.font :as font]
            [clojure.java.io :as io])
  (:import (javax.swing JFrame Box JTextPane)
           (java.awt Insets)
           (javax.swing.border EmptyBorder)
           (com.den.ses WrapEditorKit)))


(defonce title (atom "The R.E.P.L."))

(defonce main-frame (atom nil))


(defn styled-text*
  [& {:as opts}]
  (apply-options (JTextPane.) opts))


(defn- create-editor
  []
  (let [editor (styled-text* :id :editor-text-area
                             :font (font/font :name :monospaced :size 12)
                             :background "#F5EEDF")
        _      (.setEditorKit editor (WrapEditorKit.))
        _      (.setMargin editor (Insets. 5 5 5 5))
        ]
    editor))


(defn- create-repl
  []
  (let [repl (styled-text* :id :repl-text-area
                           :font (font/font :name :monospaced :size 12)
                           :background "#F5EEDF"
                           :editable? false)
        ;_    (.setTabSize repl 4)
        _    (.setMargin repl (Insets. 5 5 5 5))]
    repl))


(defn- create-splitpane-between-editor-repl
  []
  (left-right-split (scrollable (create-editor))
                    (scrollable (create-repl))
                    :divider-location 5/8))


(defn- get-icon
  [path]
  (icon/icon (io/resource path)))


(defn- create-toolbars-editor-options
  [button-size]
  [(button :id :open-file-button
           :size button-size
           :tip "Open a File"
           :icon (get-icon "open-file.png")) :separator
   (button :id :save-file-button
           :size button-size
           :icon (get-icon "save-to-file.png")) :separator
   (button :id :clear-button
           :size button-size
           :icon (get-icon "clear.png")) :separator
   (button :id :config-button
           :size button-size
           :icon (get-icon "config.png")) :separator
   (button :id :run-code-button
           :size button-size
           :icon (get-icon "run.png")) :separator])


(defn- create-toolbars-repl-options
  [toolbar* button-size]
  (.add toolbar* (Box/createHorizontalGlue))
  (.add toolbar* (button :id :repl-start-button
                         :size button-size
                         :icon (get-icon "power.png")))
  (.addSeparator toolbar*)
  (.add toolbar* (button :id :repl-clear-button
                         :size button-size
                         :icon (get-icon "clean.png")))
  (.addSeparator toolbar*)
  (.add toolbar* (button :id :repl-stop-button
                         :size button-size
                         :icon (get-icon "stop.png"))))


(defn- create-options-toolbar
  []
  (let [button-size [50 :by 50]
        toolbar*    (toolbar
                      :floatable? false
                      :items (create-toolbars-editor-options button-size))]
    (create-toolbars-repl-options toolbar* button-size)
    (.setBorder toolbar* (EmptyBorder. (Insets. 15 15 15 15)))
    toolbar*))


(defn- create-main-frame
  []
  (frame :title @title
         :content (border-panel
                    :north (create-options-toolbar)
                    :center (create-splitpane-between-editor-repl))))


(comment
  (ns-publics *ns*)
  (meta #'seesaw.rsyntax/text-area)
  (dev/show-options (styled-text))
  (dev/show-events (styled-text*))
  (select @main-frame [:#repl-stop-button]))


(defn- set-app-name!
  []
  (try
    (System/setProperty "com.apple.mrj.application.apple.menu.about.name" @title)
    (catch Exception _
      (println "There has been a problem when setting App Name."))))


(defn -main
  [& args]
  (native!)
  (set-app-name!)
  (invoke-later
    (let [frame (create-main-frame)]
      (.setExtendedState frame (bit-or (.getExtendedState frame) JFrame/MAXIMIZED_BOTH))
      (show! frame)
      (reset! main-frame frame))))



(comment
  (-main))