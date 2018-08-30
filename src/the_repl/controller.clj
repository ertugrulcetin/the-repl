(ns the-repl.controller
  (:require [seesaw.core :refer :all]
            [seesaw.event :as event]
            [seesaw.chooser :as chooser]
            [seesaw.dev :as dev]
            [the-repl.util :as util]
            [the-repl.view :as view]))


(defn- add-open-file-event
  []
  (event/listen (util/get-widget-by-id :open-file-button)
                :mouse-clicked (fn [_]
                                 (chooser/choose-file :selection-mode :files-only
                                                      :success-fn (fn [fc file]
                                                                    (text! (util/get-widget-by-id :editor-text-area)
                                                                           (slurp file)))))))


(defn- add-clean-editor-event
  []
  (event/listen (util/get-widget-by-id :clear-button)
                :mouse-clicked (fn [_]
                                 (text! (util/get-widget-by-id :editor-text-area)
                                        "(ns the-repl)\n\n")
                                 (.requestFocus (util/get-widget-by-id :editor-text-area)))))