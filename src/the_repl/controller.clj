(ns the-repl.controller
  (:require [seesaw.core :refer :all]
            [seesaw.chooser :as chooser]
            [seesaw.dev :as dev]
            [the-repl.util :as util]
            [the-repl.view :as view]
            [the-repl.core :as core]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [kezban.core :refer :all]))


(defn- append-to-repl
  [s]
  (.append (util/get-widget-by-id :repl-text-area) s))


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
                           (text! (util/get-widget-by-id :editor-taext-area)
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


(defn register-editor-events
  []
  (add-open-file-event)
  (add-clean-editor-event)
  (add-save-file-event))

(comment
  (add-run-code-event))
