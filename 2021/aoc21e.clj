(ns user
  (:require [clojure.string :as string]
            [clojure.test :refer [is run-tests]]))

(defmacro examples [f & coll-arguments-result]
  `(fn []
     ~@(for [[arguments result] (partition 2 coll-arguments-result)]
         `(is (= ~(cons f arguments) ~result)))))

(def demo-lines
  [[0,9 5,9]
   [8,0 0,8]
   [9,4 3,4]
   [2,2 2,1]
   [7,0 7,4]
   [6,4 2,0]
   [0,9 2,9]
   [3,4 1,4]
   [0,0 8,8]
   [5,5 8,2]])

(def ^:const ^long diameter 1000)

(defn render-line [grid [^long x1, ^long y1, ^long x2, ^long y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        n  (max (Math/abs dx) (Math/abs dy))
        
        dx (quot dx n)
        dy (quot dy n)
        di (+ dx (* diameter dy))]
    
    (loop [i (+ x1 (* diameter y1))
           n (inc n)]
      (when-not (zero? n)
        (->>
          (aget grid i)
          inc
          (aset grid i))
        (recur
          (+ i di)
          (dec n))))))

(defn render-lines [lines]
  (let [grid (int-array (* diameter diameter))]
    (doseq [line lines]
      (render-line grid line))
    grid))

(defn part-2
  {:test (examples part-2 [demo-lines] 12)}
  [lines]
  (transduce
    (map (fn [^long overlaps] (if (>= overlaps 2) 1 0)))
    +
    0
    (render-lines lines)))

(defn diagonal?
  {:test (examples diagonal?
           ([1,1 1,3]) false
           ([9,7 7,7]) false
           ([1,1 3,3]) true
           ([9,7 7,9]) true)}
  [[x1 y1 x2 y2]]
  (and 
    (not= x1 x2)
    (not= y1 y2)))

(defn part-1
  {:test (examples part-1 [demo-lines] 5)}
  [lines]
  (part-2 (remove diagonal? lines)))

(run-tests)
