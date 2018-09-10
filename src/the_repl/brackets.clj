(ns the-repl.brackets
  (:require [clojure.string :as str]
            [kezban.core :refer :all]
            [clojure.core.reducers :as r]))


(defonce indices-map (atom {}))


(defn get-comment-idxs
  [all-chars-indices char-index-vec]
  (let [k    (filter (fn [[e _]] (#{\; \newline} e)) char-index-vec)
        k    (partition-by (fn [[c _]] c) k)
        [[[c _]]] k
        k    (if (= c \newline) (rest k) k)
        k    (partition-all 2 k)
        size (count all-chars-indices)]
    (map (fn [[[[_ start-i]] [[_ end-i]]]]
           [start-i (or end-i size)]) k)))


(defn get-number-idxs
  [all-chars-indices char-index-vec]
  (let [k (filter (fn [[e idx]]
                    (or (and (Character/isDigit ^Character e)
                             (#{\space \newline \tab \0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \( \[ \{}
                               (nth-safe all-chars-indices (dec idx))))
                        (when (#{\/ \.} (nth-safe all-chars-indices idx))
                          (Character/isDigit ^Character (nth-safe all-chars-indices (dec idx))))))
                  char-index-vec)
        v (filter (fn [[e idx]]
                    (when-let* [_ (Character/isDigit ^Character e)
                                pre-idx (dec idx)
                                c (#{\/ \.} (nth-safe all-chars-indices pre-idx))]
                               (in? [c pre-idx] k))) char-index-vec)]
    (concat k v)))


(defn get-char-idxs
  [all-chars-indices char-index-vec]
  (let [idxs (filter (fn [[e _]] (= \\ e)) char-index-vec)]
    (map (fn [[e idx]]
           (let [char-token (apply str (map (fn [i]
                                              (nth all-chars-indices i nil))
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



(defn get-double-quote-idx
  [all-chars-indices char-index-vec]
  (partition 2 (reduce (fn [r [e i]]
                         (if (and (= \" e) (not= \\ (nth-safe all-chars-indices (dec i))))
                           (conj r i)
                           r))
                       []
                       char-index-vec)))


(defn get-keyword-idxs
  [all-chars-indices char-index-vec]
  (let [colon (filter (fn [[e _]] (= \: e)) char-index-vec)]
    (filter #(not= (first %) (second %))
            (map (fn [[_ i]]
                   (let [start-idx     i
                         keyword-chars (take-while #(not (#{\newline \space \tab \~ \@ \( \) \[ \] \{ \}} %))
                                                   (drop start-idx all-chars-indices))]
                     (cond
                       (not (first keyword-chars))
                       [0 0]

                       :else
                       [start-idx (+ start-idx (count keyword-chars))])))
                 colon))))


;;TODO fix performence problem!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(defn- find-bracket-match-map
  [open-char close-char brackets]
  (let [stack     (java.util.Stack.)
        index-map (reduce
                    (fn [m [c idx]]
                      (let [pre-val (try (.peek stack) (catch Exception _))
                            [pre-c pre-idx] pre-val]
                        (if (= c close-char)
                          (if (= pre-c open-char)
                            (do
                              (.pop stack)
                              (assoc! m pre-idx idx))
                            (do
                              (.push stack [c idx])
                              m))
                          (do
                            (.push stack [c idx])
                            m))))
                    (transient {}) brackets)]
    (persistent! index-map)))


(defn get-indices-range-set
  [range-vec]
  (set (mapcat (fn [[s e]] (range (inc s) e)) range-vec)))


(defn- get-eliminated-indexed-seq-chars-vec
  [indices-map]
  (let [open-close-char-indices-set (:open-close-char-indices-set indices-map)
        double-quote-indices-set    (get-indices-range-set (:double-quote-indices indices-map))
        comment-quote-indices       (get-indices-range-set (:comment-quote-indices indices-map))
        parens-index-vec            (:parens-index-vec indices-map)
        r                           (filter (fn [[_ i]]
                                              (and (not (double-quote-indices-set i))
                                                   (not (comment-quote-indices i))
                                                   (not (open-close-char-indices-set i))))
                                            parens-index-vec)]
    r))


(defn- get-brackets-idxs-by-type
  [brackets indices-map]
  (filter (fn [[e _]] ((set brackets) e)) (get-eliminated-indexed-seq-chars-vec indices-map)))


(defn get-match-brackets-indices-map
  [indices-map]
  (let [bracket-types         [[\( \)] [\[ \]] [\{ \}]]
        parens                (pmap #(get-brackets-idxs-by-type % indices-map) bracket-types)
        [parenthesis _ _] parens
        open-brackets-indices (map (fn [[_ i]] i) (filter (fn [[e _]] (#{\(} e)) parenthesis))
        m                     (pmap (fn [[open-c close-c] brackets]
                                      (find-bracket-match-map open-c close-c brackets)) bracket-types parens)
        m                     (into {} (apply concat m))]
    {:open-bracket-indices open-brackets-indices
     :open                 m
     :close                (clojure.set/map-invert m)}))


(defn get-fn-highlighting-indices
  [all-chars-indices match-brackets-indices-map]
  (let [xf (comp (map (fn [i]
                        (let [start-idx (inc i)
                              xf        (comp (drop start-idx)
                                              (take-while #(not (#{\newline \space \tab \~ \# \' \@ \) \] \}} %))))
                              fn-chars  (transduce xf conj all-chars-indices)]
                          (cond
                            (or (not (first fn-chars))
                                (Character/isDigit ^Character (first fn-chars)))
                            [0 0]

                            :else
                            [start-idx (+ start-idx (count fn-chars))]))))
                 (filter (fn [x] (not= (first x) (second x)))))]
    (transduce xf conj (:open-bracket-indices match-brackets-indices-map))))


(defn generate-indices!
  [code]
  (reset! indices-map (letm [all-chars-indices (vec (seq code))
                             char-index-vec (keep-indexed (fn [i e] [e i]) all-chars-indices)
                             char-indices (get-char-idxs all-chars-indices char-index-vec)
                             number-indices (get-number-idxs all-chars-indices char-index-vec)
                             keyword-indices (get-keyword-idxs all-chars-indices char-index-vec)
                             double-quote-indices (get-double-quote-idx all-chars-indices char-index-vec)
                             comment-quote-indices (get-comment-idxs all-chars-indices char-index-vec)
                             match-brackets-indices (get-match-brackets-indices-map
                                                      {:char-indices                char-indices
                                                       :double-quote-indices        double-quote-indices
                                                       :comment-quote-indices       comment-quote-indices
                                                       :all-chars-indices           all-chars-indices
                                                       :char-index-vec              char-index-vec
                                                       :parens-index-vec            (filter (fn [[e _]] (#{\( \) \[ \] \{ \}} e)) char-index-vec)
                                                       :open-close-char-indices-set (set (map (fn [[_ idx _]] (inc idx))
                                                                                              (filter (fn [[_ _ len]] (= 2 len)) char-indices)))})
                             fn-hi-indices (get-fn-highlighting-indices all-chars-indices match-brackets-indices)
                             ])))