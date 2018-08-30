(ns the-repl.view
  (:require [seesaw.core :refer :all]
            [seesaw.dev :as dev]
            [seesaw.icon :as icon]
            [clojure.java.io :as io])
  (:import (javax.swing JFrame Box)
           (java.awt Insets)
           (javax.swing.border EmptyBorder)))


(defonce title (atom "The R.E.P.L."))

(defonce main-frame (atom nil))


(defn- create-editor
  []
  (let [editor (text :multi-line? true
                     :font (seesaw.font/font :name :monospaced :size 11)
                     :wrap-lines? true)
        _      (.setTabSize editor 4)]
    editor))


(defn- create-repl
  []
  (text :multi-line? true
        :font (seesaw.font/font :name :monospaced :size 11)
        :editable? false))


(defn- create-splitpane-between-editor-repl
  []
  (left-right-split (scrollable (create-editor))
                    (scrollable (create-repl))
                    :divider-location 5/8))


;;TODO add tooltip to ertu's seesaw
(defn- create-options-toolbar
  []
  (let [button-size [50 :by 50]
        toolbar     (toolbar
                      :floatable? false
                      :items [(button :id :open-file-button
                                      :size button-size
                                      :icon (icon/icon (io/resource "open-file.png"))) :separator
                              (button :id :save-button
                                      :size button-size
                                      :icon (icon/icon (io/resource "save-to-file.png"))) :separator
                              (button :id :clear-button
                                      :size button-size
                                      :icon (icon/icon (io/resource "clear.png"))) :separator
                              (button :id :config-button
                                      :size button-size
                                      :icon (icon/icon (io/resource "config.png")))])]
    (.setBorder toolbar (EmptyBorder. (Insets. 5 5 5 5)))
    (.add toolbar (Box/createHorizontalGlue))
    (.add toolbar (button :id :repl-run-button
                          :size button-size
                          :icon (icon/icon (io/resource "run.png"))))
    (.addSeparator toolbar)
    (.add toolbar (button :id :repl-clear-button
                          :size button-size
                          :icon (icon/icon (io/resource "clean.png"))))
    (.addSeparator toolbar)
    (.add toolbar (button :id :repl-stop-button
                          :size button-size
                          :icon (icon/icon (io/resource "stop.png"))))
    toolbar))


(defn- create-main-frame
  []
  (frame :title @title
         :content (border-panel
                    :north (create-options-toolbar)
                    :center (create-splitpane-between-editor-repl))))


(comment
  (dev/show-options (text))
  (dev/show-events (frame))
  (select @main-frame [:#repl-stop-button]))


(defn -main
  [& args]
  (native!)
  (invoke-later
    (let [frame (create-main-frame)]
      (.setExtendedState frame (bit-or (.getExtendedState frame) JFrame/MAXIMIZED_BOTH))
      (show! frame)
      (reset! main-frame frame))))


(comment
  (-main))