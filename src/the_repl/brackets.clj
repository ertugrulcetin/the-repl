(ns the-repl.brackets)


(def bracket-open-close-indices (atom {}))


(defn create-match-brackets-indices-map
  [code]
  (let [seq-chars (vec (seq code))
        parens    (filter (fn [[e _]] (#{\( \)} e)) (keep-indexed (fn [i e] [e i]) seq-chars))
        p         (partition 2 (partition-by (fn [[e _]] e) parens))
        m         (apply merge (for [x p]
                                 (into {} (map (fn [[_ i1] [_ i2]]
                                                 [i1 i2]) (first x) (reverse (second x))))))]
    (reset! bracket-open-close-indices {:open m :close (clojure.set/map-invert m)})))
