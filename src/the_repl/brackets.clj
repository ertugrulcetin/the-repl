(ns the-repl.brackets)


(def code-char-indices (atom []))

(def bracket-open-close-indices (atom {}))


(defn create-match-brackets-indices-map
  [code]
  (let [seq-chars             (vec (seq code))
        parens                (filter (fn [[e _]] (#{\( \)} e)) (keep-indexed (fn [i e] [e i]) seq-chars))
        open-brackets-indices (map (fn [[_ i]] i) (filter (fn [[e _]] (#{\(} e)) parens))
        m                     (into {} (loop [d (vec parens)
                                              r #{}]
                                         (if (seq d)
                                           (let [vv               (filter (fn [[[p1 i1] [p2 i2]]]
                                                                            (when (and (= \( p1) (= \) p2))
                                                                              [[p1 i1] [p2 i2]])) (map (fn [x y]
                                                                                                         [x y]) d (rest d)))
                                                 indices          (map (fn [[[_ idx1] [_ idx2]]]
                                                                         [idx1 idx2]) vv)
                                                 removed-elements (set (apply concat vv))]
                                             (recur (remove removed-elements d) (clojure.set/union r (set indices))))
                                           r)))]
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

@bracket-open-close-indices
(count (apply str @code-char-indices))