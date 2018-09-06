(ns the-repl.brackets)



(def src-indexes (atom nil))


(defn create-src-vec
  [code]
  (reset! src-indexes (vec (seq code))))


(defn get-bracket-indexes-map
  []
  (map-indexed (fn [i e] [i e]) @src-indexes))


(defn brackets-match?
  [start-i end-i]
  (reverse (take end-i @src-indexes))
  )


(create-src-vec "(flatten (for [open-p-i [50 42 30]]\n                   (for [close-p-i [35 48 59]\n                         :when (< open-p-i close-p-i)]\n                     {:open-close-index [open-p-i close-p-i]\n                      :diff             (Math/abs (- open-p-i close-p-i))})))")

(= "oo" "\"deneme\"")
(vec (seq "(= \"oo\" \"\\\"deneme\\\"\")"))

(reduce (fn [m [i e]]
          (cond
            (= \( e)
            (update m \( conj i)

            (= \) e)
            (update m \) conj i)

            :else
            m)) {}
        (get-bracket-indexes-map))


(def m {"(" [50 42 30]
        ")" [35 48 59]})

(reduce (fn [result v]
          ) [] (get ")" m))


(def close-p-i (atom (apply sorted-set-by (cons (fn [a b] (> a b)) [35 48 59]))))

(def diff (atom nil))

(reduce (fn [result v]
          (clojure.set/union result #{[v (reduce (fn [_ c]
                                                   (let [d (Math/abs (- v c))]
                                                     (println "V: " v " C: " c " Diff: " d)
                                                     (if (or (not @diff) (< d @diff))
                                                       (do
                                                         (reset! diff d)
                                                         nil)
                                                       (do
                                                         (println "C: " c)
                                                         (reset! diff nil)
                                                         (swap! close-p-i clojure.set/difference #{c})
                                                         (reduced c))))) nil @close-p-i)]}))
        #{}
        (apply sorted-set-by (cons (fn [a b] (> a b)) [50 42 30])))


(time (dotimes [_ 1] (doall (for [open-p-i  (range 0 1000)
                                  close-p-i (range 1000 2000)
                                  :when (< open-p-i close-p-i)]
                              {:open-close-index [open-p-i close-p-i]
                               :diff             (Math/abs (- open-p-i close-p-i))}))))

(comment (time (dotimes [_ 10]
                 (vals (reduce (fn [result-m {:keys [open-close-index]}]
                                 (if (get result-m (first open-close-index))
                                   result-m
                                   (assoc result-m (first open-close-index) open-close-index)))
                               {}
                               (sort-by :diff rr))))))

