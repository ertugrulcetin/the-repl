(ns the-repl.brackets
  (:require [clojure.string :as str]
            [kezban.core :refer :all]))


(def code-char-indices (atom []))

(def bracket-open-close-indices (atom {}))

(def double-quote-indices (atom ()))


(defn get-comment-idxs
  []
  (let [s    (keep-indexed (fn [i e] [e i]) @code-char-indices)
        k    (filter (fn [[e _]] (#{\; \newline} e)) s)
        k    (partition-by (fn [[c _]] c) k)
        [[[c _]]] k
        k    (if (= c \newline) (rest k) k)
        k    (partition-all 2 k)
        size (count @code-char-indices)]
    (map (fn [[[[_ start-i]] [[_ end-i]]]]
           [start-i (or end-i size)]) k)))


(defn get-number-idxs
  []
  (let [s (keep-indexed (fn [i e] [e i]) @code-char-indices)
        k (filter (fn [[e idx]]
                    (or (and (Character/isDigit ^Character e)
                             (#{\space \newline \tab \0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \( \[ \{}
                               (nth-safe @code-char-indices (dec idx))))
                        (when (#{\/ \.} (nth-safe @code-char-indices idx))
                          (Character/isDigit ^Character (nth-safe @code-char-indices (dec idx))))))
                  s)
        v (filter (fn [[e idx]]
                    (when-let* [_ (Character/isDigit ^Character e)
                                pre-idx (dec idx)
                                c (#{\/ \.} (nth-safe @code-char-indices pre-idx))]
                               (in? [c pre-idx] k))) s)]
    (concat k v)))


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



(defn- get-eliminated-indexed-seq-chars-vec
  [seq-chars]
  (let [open-close-char-indices-set (set (map (fn [[_ idx _]] (inc idx)) (filter (fn [[_ _ len]] (= 2 len)) (get-char-idxs))))]
    (filter (fn [[_ i]]
              (and (not (loop [[f & others :as d-indices] @double-quote-indices
                               result false]
                          (if (seq d-indices)
                            (recur others (or result (and (> i (first f)) (< i (second f)))))
                            result)))
                   (not (open-close-char-indices-set i))))
            (keep-indexed (fn [i e] [e i]) seq-chars))))


(defn- get-brackets-idxs-by-type
  [brackets seq-chars]
  (filter (fn [[e _]] ((set brackets) e)) (get-eliminated-indexed-seq-chars-vec seq-chars)))


(defn get-double-quote-idx
  []
  (partition 2 (reduce (fn [r [e i]]
                         (if (and (= \" e) (not= \\ (nth @code-char-indices (dec i) nil)))
                           (conj r i)
                           r))
                       []
                       (keep-indexed (fn [i e] [e i]) @code-char-indices))))


(defn get-keyword-idxs
  []
  (let [colon (filter (fn [[e _]] (= \: e)) (keep-indexed (fn [i e] [e i]) @code-char-indices))]
    (filter #(not= (first %) (second %))
            (map (fn [[_ i]]
                   (let [start-idx     i
                         keyword-chars (take-while #(not (#{\newline \space \tab \~ \@ \( \) \[ \] \{ \}} %))
                                                   (drop start-idx @code-char-indices))]
                     (cond
                       (not (first keyword-chars))
                       [0 0]

                       :else
                       [start-idx (+ start-idx (count keyword-chars))])))
                 colon))))


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