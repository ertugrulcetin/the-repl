(ns the-repl.brackets)


(def code-char-indices (atom []))

(def bracket-open-close-indices (atom {}))


(defn create-match-brackets-indices-map
  [code]
  (let [seq-chars             (vec (seq code))
        parens                (filter (fn [[e _]] (#{\( \) \[ \] \{ \}} e)) (keep-indexed (fn [i e] [e i]) seq-chars))
        open-brackets-indices (map (fn [[_ i]] i) (filter (fn [[e _]] (#{\(} e)) parens))
        p                     (partition 2 (partition-by (fn [[e _]] e) parens))
        m                     (apply merge (for [x p]
                                             (into {} (map (fn [[_ i1] [_ i2]]
                                                             [i1 i2]) (first x) (reverse (second x))))))]
    (reset! code-char-indices seq-chars)
    (reset! bracket-open-close-indices {:open-bracket-indices open-brackets-indices
                                        :open                 m
                                        :close                (clojure.set/map-invert m)})))


(defn get-fn-highlighting-indices
  []
  (filter #(not= (first %) (second %))
          (map (fn [i]
                 (let [start-idx (inc i)
                       fn-chars  (take-while #(not (#{\newline \space \tab \~} %))
                                             (drop start-idx @code-char-indices))]
                   (cond
                     (Character/isDigit ^Character (first fn-chars))
                     [0 0]

                     :else
                     [start-idx (+ start-idx (count fn-chars))])))
               (:open-bracket-indices @bracket-open-close-indices))))