(ns the-repl.brackets)


(def src-indexes (atom nil))

(def bracket-open-close-indices (atom {}))


(defn create-src-vec
  [code]
  (reset! src-indexes (vec (seq code))))


(defn create-match-brackets-indices-map
  []
  (let [parens (filter (fn [[e _]]
                         (#{\( \)} e))
                       (keep-indexed (fn [i e] [e i]) @src-indexes))
        p      (partition 2 (partition-by (fn [[e _]] e) parens))
        m      (apply merge (for [x p]
                              (into {} (map (fn [[_ i1] [_ i2]]
                                              [i1 i2]) (first x) (reverse (second x))))))]
    (reset! bracket-open-close-indices {:open  m
                                        :close (clojure.set/map-invert m)})))