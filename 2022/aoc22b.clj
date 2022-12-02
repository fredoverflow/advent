(ns user
  (:require [clojure.string :as string]
            [clojure.test :refer [do-report run-tests]]))


(defn- filter-stack-trace! [^Throwable throwable]
  (->> (for [^StackTraceElement element (. throwable getStackTrace)
             :when (. "clopad.txt" equals (. element getFileName))]
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

; The first column is what your opponent is going to play:
; A for Rock, B for Paper, and C for Scissors.

; The second column, you reason, must be what you should play in response:
; X for Rock, Y for Paper, and Z for Scissors.

(def example "A Y
  B X
  C Z")

(defpure split-lines
  {[example] ["A Y"
              "B X"
              "C Z"]}
  "Splits s into lines. Trims leading space."
  [^String s]
  (string/split s #"\n *"))

; The score for a single round is the score for the shape you selected
; (1 for Rock, 2 for Paper, and 3 for Scissors)

(def ROCK     1)
(def PAPER    2)
(def SCISSORS 3)

; plus the score for the outcome of the round
; (0 if you lost, 3 if the round was a draw, and 6 if you won).

(def LOSE     0)
(def DRAW     3)
(def WIN      6)

(def scores1 {;ROCK
              ;|
              "A X" (+ ROCK     DRAW)
              "A Y" (+ PAPER    WIN)
              "A Z" (+ SCISSORS LOSE)
              
              ;PAPER
              ;|
              "B X" (+ ROCK     LOSE)
              "B Y" (+ PAPER    DRAW)
              "B Z" (+ SCISSORS WIN)
              
              ;SCISSORS
              ;|
              "C X" (+ ROCK     WIN)
              "C Y" (+ PAPER    LOSE)
              "C Z" (+ SCISSORS DRAW)})

(defpure part1
  {[example] 15}
  "What would your total score be
  if everything goes exactly according to your strategy guide?"
  [rounds]
  (transduce (map scores1) + 0 (split-lines rounds)))

; X means you need to lose,
; Y means you need to end the round in a draw, and
; Z means you need to win.

(def scores2 {;ROCK
              ;|
              "A Y" (+ ROCK     DRAW)
              "A Z" (+ PAPER    WIN)
              "A X" (+ SCISSORS LOSE)
              
              ;PAPER
              ;|
              "B X" (+ ROCK     LOSE)
              "B Y" (+ PAPER    DRAW)
              "B Z" (+ SCISSORS WIN)
              
              ;SCISSORS
              ;|
              "C Z" (+ ROCK     WIN)
              "C X" (+ PAPER    LOSE)
              "C Y" (+ SCISSORS DRAW)})

(defpure part2
  {[example] 12}
  "What would your total score be
  if everything goes exactly according to your strategy guide?"
  [rounds]
  (transduce (map scores2) + 0 (split-lines rounds)))



(run-tests)
