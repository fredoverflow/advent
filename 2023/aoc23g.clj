(ns user
  (:require [clojure.string :as string]
            [clojure.test :refer [do-report run-tests]]))



(defn- filter-stack-trace! [^Throwable throwable]
  (->> (for [^StackTraceElement element (. throwable getStackTrace)
             :when (. *source-path* equals (. element getFileName))]
         element)
    (into-array StackTraceElement)
    (. throwable setStackTrace))
  throwable)

(defn- register-test [v, inputs->output-map, do-report]
  (alter-meta! v assoc :test
    #(doseq [[inputs output] inputs->output-map]
       (try
         (let [actual (apply @v inputs)]
           (do-report {:type     (if (= output actual) :pass :fail)
                       :message  (str "  inputs: " inputs)
                       :expected output
                       :actual   actual}))
         (catch Throwable throwable
           (do-report {:type     :error
                       :message  (str "  inputs: " inputs)
                       :expected output
                       :actual   (filter-stack-trace! throwable)}))))))

(defmacro defpure
  "Defines a pure function, illustrated by an exemplary inputs->output map"
  [name, inputs->output-map & rest]
  `(do
     (defn ~name ~@rest)
     (register-test (var ~name) ~inputs->output-map #(do-report %))))



(defpure hand-type1
  {["AAAAA"] [5 0 0 0 0]
   ["AA8AA"] [4 1 0 0 0]
   ["23332"] [3 2 0 0 0]
   ["TTT98"] [3 1 1 0 0]
   ["23432"] [2 2 1 0 0]
   ["A23A4"] [2 1 1 1 0]
   ["23456"] [1 1 1 1 1]}
  "Every hand is exactly one type. From strongest to weakest, they are:
  ·Five of a kind
  ·Four of a kind
  ·Full house
  ·Three of a kind
  ·Two pair
  ·One pair
  ·High card"
  [^String hand]
  (->> hand
    frequencies
    vals
    (concat [0 0 0 0 0])
    (sort >)
    (into [] (take 5)))) ; Vectors compare by length first, then content :-/

(def card-strength1
  "A hand consists of five cards labeled one of
  A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, or 2.
  The relative strength of each card follows this order,
  where A is the highest and 2 is the lowest."
  {\2  2
   \3  3
   \4  4
   \5  5
   \6  6
   \7  7
   \8  8
   \9  9
   \T 10
   \J 11
   \Q 12
   \K 13
   \A 14})

(defpure prepare-line1
  {["32T3K 765"] [[2 1 1 1 0] [3 2 10 3 13] 765]}
  "Hands are primarily ordered based on type;
  for example, every full house is stronger than any three of a kind.
  If two hands have the same type, a second ordering rule takes effect.
  Start by comparing the first card in each hand. If these cards are different,
  the hand with the stronger first card is considered stronger.
  Otherwise, continue with the second card in each hand, then third card etc."
  [^String line]
  (let [[        _                hand   bid]
        (re-find #"([23456789TJQKA]{5}) (\d+)" line)]
    
    [(hand-type1 hand) (mapv card-strength1 hand) (parse-long bid)]))

(defpure part1
  {["32T3K 765
    T55J5 684
    KK677 28
    KTJJT 220
    QQQJA 483"] (+ (* 765 1) (* 220 2) (* 28 3) (* 684 4) (* 483 5))}
  "Determine the total winnings of this set of hands by adding up
  the result of multiplying each hand's bid with its rank."
  [^String lines]
  (->> lines
    string/split-lines
    (mapv prepare-line1)
    sort
    (transduce
      (map-indexed (fn [index [_ _ bid]] (* bid (inc index))))
      + 0)))



(defn hand-type2
  "Now, J cards are jokers - wildcards that can act like
  whatever card would make the hand the strongest type possible."
  [^String hand]
  (let [sans-jokers (.replace hand "J" "")
        joker-count (- 5 (.length sans-jokers))]
    
    (update (hand-type1 sans-jokers) 0 (partial + joker-count))))

(def card-strength2
  "J cards are now the weakest individual cards, weaker even than 2."
  (assoc card-strength1 \J 1))

(defn prepare-line2
  [^String line]
  (let [[        _                hand   bid]
        (re-find #"([23456789TJQKA]{5}) (\d+)" line)]
    
    [(hand-type2 hand) (mapv card-strength2 hand) (parse-long bid)]))

(defpure part2
  {["32T3K 765
    T55J5 684
    KK677 28
    KTJJT 220
    QQQJA 483"] (+ (* 765 1) (* 28 2) (* 684 3) (* 483 4) (* 220 5))}
  [^String lines]
  (->> lines
    string/split-lines
    (mapv prepare-line2)
    sort
    (transduce
      (map-indexed (fn [index [_ _ bid]] (* bid (inc index))))
      + 0)))



(run-tests)
