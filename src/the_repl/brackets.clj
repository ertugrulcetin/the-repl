(ns the-repl.brackets
  (:require [clojure.string :as str]))


(def code-char-indices (atom []))

(def bracket-open-close-indices (atom {}))

(def double-quote-indices (atom ()))


(defn- get-eliminated-indexed-seq-chars-vec
  []
  (filter (fn [[_ i]]
            (loop [[f & others :as d-indices] @double-quote-indices
                   result false]
                  (if (seq d-indices)
                    (recur others (or result (and (> i (first f)) (< i (second f)))))
                    result))) (keep-indexed (fn [i e] [e i]) @code-char-indices)))


(defn- get-brackets-idxs-by-type
  [brackets seq-chars]
  (filter (fn [[e _]] ((set brackets) e)) (keep-indexed (fn [i e] [e i]) seq-chars)))


(defn get-double-quote-idx
  []
  (partition 2 (reduce (fn [r [e i]]
                         (if (and (= \" e) (not= \\ (nth @code-char-indices (dec i) nil)))
                           (conj r i)
                           r))
                       []
                       (keep-indexed (fn [i e] [e i]) @code-char-indices))))
(get-double-quote-idx)

(defn get-keyword-idxs
  []
  (let [colon (filter (fn [[e _]] (= \: e)) (keep-indexed (fn [i e] [e i]) @code-char-indices))]
    (map (fn [[_ i]]
           (let [start-idx     i
                 keyword-chars (take-while #(not (#{\newline \space \tab \~ \@ \( \) \[ \] \{ \}} %))
                                           (drop start-idx @code-char-indices))]
             (cond
               (not (first keyword-chars))
               [0 0]

               :else
               [start-idx (+ start-idx (count keyword-chars))])))
         colon)))


(defn get-char-idxs
  []
  (let [idxs (filter (fn [[e _]] (= \\ e)) (keep-indexed (fn [i e] [e i]) @code-char-indices))]
    (map (fn [[e idx]]
           (let [char-token (apply str (map (fn [i]
                                              (nth @code-char-indices i nil))
                                            (range (inc idx) (+ idx (inc (count "backspace"))))))]
             (cond
               (str/starts-with? char-token "space")
               [e idx (inc (count "space"))]

               (str/starts-with? char-token "newline")
               [e idx (inc (count "newline"))]

               (str/starts-with? char-token "tab")
               [e idx (inc (count "tab"))]

               (str/starts-with? char-token "backspace")
               [e idx (inc (count "backspace"))]

               (str/starts-with? char-token "formfeed")
               [e idx (inc (count "formfeed"))]

               (str/starts-with? char-token "return")
               [e idx (inc (count "return"))]

               :else
               [e idx 2]))) idxs)))


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
        _                     (reset! code-char-indices seq-chars)
        _                     (reset! double-quote-indices (get-double-quote-idx))
        bracket-types         [[\( \)] [\[ \]] [\{ \}]]
        parens                (filter seq (map #(get-brackets-idxs-by-type % seq-chars) bracket-types))
        [parenthesis _ _] parens
        open-brackets-indices (map (fn [[_ i]] i) (filter (fn [[e _]] (#{\(} e)) parenthesis))
        m                     (mapcat (fn [[open-c close-c] brackets]
                                        (find-bracket-match-map open-c close-c brackets)) bracket-types parens)
        m                     (into {} m)]
    (reset! bracket-open-close-indices {:open-bracket-indices open-brackets-indices
                                        :open                 m
                                        :close                (clojure.set/map-invert m)})))


(defn get-fn-highlighting-indices
  []
  (filter #(not= (first %) (second %))
          (map (fn [i]
                 (let [start-idx (inc i)
                       fn-chars  (take-while #(not (#{\newline \space \tab \~ \# \' \@ \) \] \}} %))
                                             (drop start-idx @code-char-indices))]
                   (cond
                     (or (not (first fn-chars))
                         (Character/isDigit ^Character (first fn-chars)))
                     [0 0]

                     :else
                     [start-idx (+ start-idx (count fn-chars))])))
               (:open-bracket-indices @bracket-open-close-indices))))