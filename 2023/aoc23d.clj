(ns user
  (:require [clojure.string :as string]
            [clojure.set :refer [intersection]]
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



(defpure parse-numbers
  {["41 48 83 86 17"]
   #{41 48 83 86 17}}
  [^String numbers]
  (into #{} (map parse-long) (re-seq #"\d+" numbers)))

(defpure linear-score
  {["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"] 4
   ["Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"] 2
   ["Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"] 1
   ["Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"] 0}
  [^String card]
  (let [[_ winning-numbers my-numbers] (string/split card #"[:|]")]
    (count
      (intersection
        (parse-numbers winning-numbers)
        (parse-numbers my-numbers)))))

(defpure exponential-score-halved
  {["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"] 8
   ["Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"] 2
   ["Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"] 1
   ["Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"] 0}
  
  "The first match makes the card worth one point
  and each match after the first doubles the point value of that card."
  [^String card]
  (bit-shift-right
    (bit-shift-left 1 (linear-score card)) ; 2^linear-score
    1))                                    ; /2

(defpure part1
  {["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"]
   (+ 8 2 2 1 0 0)}
  "How many points are the cards worth in total?"
  [^String cards]
  (transduce (map exponential-score-halved) + 0 (string/split-lines cards)))



(defpure part2
  {["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"]
   30}
  "There's no such thing as 'points'.
  Instead, scratchcards only cause you to win more scratchcards
  equal to the number of winning numbers you have."
  [^String cards]
  (let [cards  (string/split-lines cards)
        length (count cards)
        copies (int-array (repeat length 1))]
    (loop [index 0]
      (if (< index length)
        (let [card  (cards index)
              copy  (aget copies index)
              begin (inc index)
              end   (+ begin (linear-score card))]
          "Specifically, you win copies of the scratchcards
          below the winning card equal to the number of matches."
          (doseq [i (range begin end)]
            (aset copies i (+ (aget copies i) copy)))
          
          (recur begin))
        (reduce + 0 copies)))))



(run-tests)
