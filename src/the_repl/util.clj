(ns the-repl.util
  (:require [seesaw.core :refer :all]
            [the-repl.view :as view])
  (:import (java.awt Color)
           (javax.swing.text StyleConstants SimpleAttributeSet)))


(defn get-widget-by-id
  [id]
  (select @view/main-frame [(keyword (str "#" (name id)))]))



;(let [sas    (SimpleAttributeSet.)
;      sd     (.getStyledDocument (get-widget-by-id :editor-text-area))
;      _      (StyleConstants/setForeground sas Color/BLACK)]
;  (.setCharacterAttributes sd 10 25 sas true))

