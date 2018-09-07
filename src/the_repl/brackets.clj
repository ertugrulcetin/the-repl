(ns the-repl.brackets)


(def code-char-indices (atom []))

(def bracket-open-close-indices (atom {}))


(defn- get-brackets-idxs-by-type
  [brackets seq-chars]
  (filter (fn [[e _]] ((set brackets) e)) (keep-indexed (fn [i e] [e i]) seq-chars)))


(defn- find-bracket-match-map
  [open-char close-char brackets]
  (into {} (loop [d brackets
                  r #{}]
             (if (seq d)
               (let [vv                (filter (fn [[[p1 i1] [p2 i2]]]
                                                 (when (and (= open-char p1) (= close-char p2))
                                                   [[p1 i1] [p2 i2]])) (map (fn [x y]
                                                                              [x y]) d (rest d)))
                     indices           (map (fn [[[_ idx1] [_ idx2]]]
                                              [idx1 idx2]) vv)
                     found-elements    (set (apply concat vv))
                     remained-elements (remove found-elements d)
                     remained-elements (if (empty? found-elements) [] remained-elements)]
                 (recur remained-elements (clojure.set/union r (set indices))))
               r))))

(defn create-match-brackets-indices-map
  [code]
  (let [seq-chars             (vec (seq code))
        bracket-types         [[\( \)] [\[ \]] [\{ \}]]
        parens                (filter seq (map #(get-brackets-idxs-by-type % seq-chars) bracket-types))
        open-brackets-indices (map (fn [[_ i]] i) (filter (fn [[e _]] (#{\(} e)) parens))
        m                     (mapcat (fn [[open-c close-c] brackets]
                                        (find-bracket-match-map open-c close-c brackets)) bracket-types parens)
        m                     (into {} m)]
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

