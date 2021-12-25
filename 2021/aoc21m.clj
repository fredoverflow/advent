(ns user
  (:require [clojure.string :as string]
            [clojure.test :refer [is run-tests]]))

(defmacro examples [f & coll-arguments-result]
  `(fn []
     ~@(for [[arguments result] (partition 2 coll-arguments-result)]
         `(is (= ~(cons f arguments) ~result)))))

(def demo-paper
  {:dots
   #{[6 10]
     [0 14]
     [9 10]
     [0 3]
     [10 4]
     [4 11]
     [6 0]
     [6 12]
     [4 1]
     [0 13]
     [10 12]
     [3 4]
     [3 0]
     [8 4]
     [1 10]
     [2 14]
     [8 10]
     [9 0]}
   :folds
   [[:y 7]
    [:x 5]]})

(defn fold-once
  {:test (examples fold-once
           [#{[5 7] [11 13]} [:x 10]]
           #{ [5 7] [ 9 13]}
           
           [#{[5 7] [11 13]} [:y 10]]
           #{ [5 7] [11  7]})}
  [dots [axis position]]
  (into #{}
    (map
      (case axis
        :x (fn [[x y]] (if (<= x position)
                         [x y]
                         [(- (* 2 position) x) y]))
        
        :y (fn [[x y]] (if (<= y position)
                         [x y]
                         [x (- (* 2 position) y)]))))
    dots))

(defn part-1
  "How many dots are visible after completing just the first fold?"
  {:test (examples part-1 [demo-paper] 17)}
  [{dots :dots, [first-fold] :folds}]
  (count
    (fold-once dots first-fold)))

(defn render
  {:test (examples render [#{[2 3] [5 1]}]
           (-> "
             ......
             .....#
             ......
             ..#..." (string/replace " " "") string/trim))}
  [dots]
  (let [max-x (apply max (map first  dots))
        max-y (apply max (map second dots))]
    (string/join "\n"
      (for [y (range (inc max-y))]
        (apply str
          (for [x (range (inc max-x))]
            (if (dots [x y]) \# \.)))))))

(defn part-2
  "Finish folding the transparent paper"
  {:test (examples part-2 [demo-paper]
           (-> "
             #####
             #...#
             #...#
             #...#
             #####" (string/replace " " "") string/trim))}
  [{dots :dots, folds :folds}]
  (render
    (reduce
      fold-once
      dots
      folds)))

(run-tests)
