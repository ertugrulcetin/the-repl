(ns the-repl.util
  (:require [seesaw.core :refer :all]
            [the-repl.view :as view]))


(defn get-widget-by-id
  [id]
  (select @view/main-frame [(keyword (str "#" (name id)))]))