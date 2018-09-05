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

(time (dotimes [_ 10000] (doall (get-bracket-indexes-map))))

(def m {"(" [50 42 30]
        ")" [35 48 59]})

(reduce (fn [result v]
          ) [] (get ")" m))


(defn split-equally
  [num coll]
  "Split a collection into a vector of (as close as possible) equally sized parts"
  (loop [num   num
         parts []
         coll  coll
         c     (count coll)]
    (if (<= num 0)
      parts
      (let [t (quot (+ c num -1) num)]
        (recur (dec num) (conj parts (take t coll)) (drop t coll) (- c t))))))

(defmacro dopar
  [thread-count [sym coll] & body]
  `(doall (pmap
            (fn [vals#]
              (for [~sym vals#]
                ~@body))
            (split-equally ~thread-count ~coll))))

(def rr (dopar 5 [open-p-i (range 0 1000)]
               (dopar 5 [close-p-i (range 1000 2000)]
                      {:open-close-index [open-p-i close-p-i]
                       :diff             (Math/abs (- open-p-i close-p-i))})))

(comment (def rr (for [open-p-i  (range 0 1000)
                       close-p-i (range 1000 2000)
                       :when (< open-p-i close-p-i)]
                   {:open-close-index [open-p-i close-p-i]
                    :diff             (Math/abs (- open-p-i close-p-i))})))

(require '[clojure.core.reducers :as r])


(time (dotimes [_ 1]
        (vals (r/reduce (fn [result-m {:keys [open-close-index]}]
                          (if (get result-m (first open-close-index))
                            result-m
                            (assoc result-m (first open-close-index) open-close-index)))
                        {}
                        (sort-by :diff rr)))))