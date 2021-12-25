(ns user
  (:require [clojure.string :as string]
            [clojure.test :refer [is run-tests]]))

(defmacro examples [f & coll-arguments-result]
  `(fn []
     ~@(for [[arguments result] (partition 2 coll-arguments-result)]
         `(is (= ~(cons f arguments) ~result)))))

(def demo-positions [16,1,2,0,4,2,7,1,2,14])

(defn distance
  {:test (examples distance
           [8 5] 3
           [5 8] 3)}
  [a b]
  (if (> a b)
    (- a b)
    (- b a)))

(defn fuel-cost-1
  {:test (examples fuel-cost-1 [demo-positions 2] 37)}
  [positions target]
  (reduce
    (fn [result item] (+ result (distance item target)))
    0
    positions))

(defn sum-1-to-n
  {:test (examples sum-1-to-n
           [0] 0
           [1] 1
           [2] 3
           [3] 6
           [4] 10
           [5] 15)}
  [n]
  (quot (* n (inc n)) 2))

(defn fuel-cost-2
  {:test (examples fuel-cost-2 [demo-positions 5] 168)}
  [positions target]
  (reduce
    (fn [result item] (+ result (sum-1-to-n (distance item target))))
    0
    positions))

(defn global-minimum
  {:test (examples global-minimum
           [demo-positions fuel-cost-1] 37
           [demo-positions fuel-cost-2] 168)}
  [positions fuel-cost-f]
  (apply min
    (for [target (range (apply min positions)
                   (inc (apply max positions)))]
      (fuel-cost-f positions target))))

(defn local-minimum
  {:test (examples local-minimum
           [demo-positions fuel-cost-1] 37
           [demo-positions fuel-cost-2] 168)}
  [positions fuel-cost-f]
  (let [sorted (vec (sort positions))]
    (loop [left  0
           right (dec (count sorted))]
      (let [middle (quot (+ left right) 2)
            pred   (fuel-cost-f sorted (dec middle))
            here   (fuel-cost-f sorted middle)
            succ   (fuel-cost-f sorted (inc middle))]
        (cond
          (< pred here) (recur left (dec middle))
          (< succ here) (recur (inc middle) right)
          :else         here)))))

(run-tests)
