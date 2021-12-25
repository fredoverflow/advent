(ns user
  (:require [clojure.string :as string]
            [clojure.test :refer [is run-tests]]))

(defmacro examples [f & coll-arguments-result]
  `(fn []
     ~@(for [[arguments result] (partition 2 coll-arguments-result)]
         `(is (= ~(cons f arguments) ~result)))))

(def demo-target
  {:x1  20
   :x2  30
   :y1  -5
   :y2 -10})

;  6                     d   e         
;  5                c           f
;  4
;  3   3      b                   g
;  2   ^
;  1   |
;  0   a----->7                    h
; -1
; -2                              TTT
; -3                              TTT
; -4                               i

(def triangular-numbers
  "[0 1 3 6 10 15 21 28 36 45 55 ... 4851 4950]"
  (vec
    (take 100
      (reductions
        +
        0
        (iterate inc 1)))))

(defn part-1
  "Find the initial velocity that causes the probe to reach the highest y position
  and still eventually be within the target area after any step.
  What is the highest y position it reaches on this trajectory?"
  {:test (examples part-1 [demo-target] 45)}
  ^long [{^long y2 :y2}]
  (triangular-numbers (- 0 y2 1)))

(defn hits-target? [{:keys [^long x1, ^long y1, ^long x2, ^long y2]}
                    ^long vx, ^long vy]
  ; The probe's x,y position starts at 0,0
  (loop [x   0
         y   0
         vx vx
         vy vy]
    (cond
      ; miss
      (or
        (> x x2)
        (< y y2)) false
      
      ; uncertain
      (or
        (< x x1)
        (> y y1)) (recur
                    ; x position increases by x velocity
                    (+ x vx)
                    ; y position increases by y velocity
                    (+ y vy)
                    ; x velocity decreases by 1 if greater than 0
                    (if (pos? vx) (dec vx) 0)
                    ; y velocity decreases by 1
                    (dec vy))
      ; hit
      :else true)))

(defn triangular-index [number]
  "0  1  2 2  3 3 3  4 4 4 4  5 5 5 5 5  6 6 6 6 6 6  7 ..."
  (let [index (java.util.Collections/binarySearch
                triangular-numbers
                number)]
    (if (neg? index)
      (bit-not index)
      index)))

(defn part-2
  "How many distinct initial velocity values cause the probe
  to be within the target area after any step?"
  {:test (examples part-2 [demo-target] 112)}
  ^long [{:keys [^long x1, ^long x2, ^long y2] :as target}]
  (count
    (for [vx (range (triangular-index x1) (inc x2))
          vy (range y2 (- y2))
          :when (hits-target? target vx vy)]
      nil)))

(run-tests)
