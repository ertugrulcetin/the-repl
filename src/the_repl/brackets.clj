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


(create-src-vec "(= \"sela\"oo\"m\" \"bebek\")")

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
