(ns user
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is run-tests]]))

(defmacro examples [f & coll-arguments-result]
  `(fn []
     ~@(for [[arguments result] (partition 2 coll-arguments-result)]
         `(is (= ~(cons f arguments) ~result)))))

(def deterministic-die
  "This die always rolls 1 first, then 2, then 3, and so on
  up to 100, after which it starts over at 1 again"
  (->> (range 1 101)
    repeat
    (apply concat)))

(defn part-1
  {:test (examples part-1
           [4 0 8 0 0 deterministic-die] (* 745 993))}
  [pos-1 score-1
   pos-2 score-2
   rolls [a b c & dice]]
  (let [rolls   (+ 3 rolls)
        ; Roll the die three times and add up the results
        ; Move the pawn that many times forward around the track
        ; wrapping back around to 1 after 10
        pos-1   (-> pos-1 (+ a b c) dec (mod 10) inc)
        ; Increase the score by the value of the space stopped on
        score-1 (+ score-1 pos-1)]
    ; The game immediately ends as a win for any player
    ; whose score reaches at least 1000
    (if (>= score-1 1000)
      ; Multiply the score of the losing player
      ; by the number of times the die was rolled during the game
      (* score-2 rolls)
      (recur pos-2 score-2 pos-1 score-1 rolls dice))))

(def distribution
  (frequencies
    (for [a (range 1 4)
          b (range 1 4)
          c (range 1 4)]
      (+ a b c))))

(defrecord Wins [^long one, ^long two])

(def zero-wins (->Wins 0 0))

(defn add-wins [^Wins a, ^Wins b]
  (->Wins (+ (.one a) (.one b)) (+ (.two a) (.two b))))

(defn mul-wins [^long factor, ^Wins w]
  (->Wins (* factor (.one w)) (* factor (.two w))))

(declare part-2b)

(def part-2
  (memoize
    (fn [pos-1 score-1,
         pos-2 long score-2]
      (reduce
        add-wins
        zero-wins
        (for [[^long dice3, ^long n] distribution
              :let [pos-1   (-> pos-1 (+ dice3) dec (mod 10) inc)
                    score-1 (+ score-1 pos-1)]]
          (if (>= score-1 21)
            (->Wins n 0)
            (mul-wins n (part-2b pos-1 score-1 pos-2 score-2))))))))

(def part-2b
  (memoize
    (fn [pos-1 score-1,
         pos-2 score-2]
      (reduce
        add-wins
        zero-wins
        (for [[^long dice3, ^long n] distribution
              :let [pos-2   (-> pos-2 (+ dice3) dec (mod 10) inc)
                    score-2 (+ score-2 pos-2)]]
          (if (>= score-2 21)
            (->Wins 0 n)
            (mul-wins n (part-2 pos-1 score-1 pos-2 score-2))))))))

(deftest part-2-test
  (is (= (part-2 4 0 8 0) (->Wins 444356092776315 341960390180808))))

(run-tests)
