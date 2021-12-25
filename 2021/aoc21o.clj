(ns user
  (:require [clojure.string :as string]
            [clojure.data.priority-map :refer [priority-map]]
            [clojure.test :refer [is run-tests]]))

(defmacro examples [f & coll-arguments-result]
  `(fn []
     ~@(for [[arguments result] (partition 2 coll-arguments-result)]
         `(is (= ~(cons f arguments) ~result)))))

(def demo-cavern
  [[1 1 6 3 7 5 1 7 4 2]
   [1 3 8 1 3 7 3 6 7 2]
   [2 1 3 6 5 1 1 3 2 8]
   [3 6 9 4 9 3 1 5 6 9]
   [7 4 6 3 4 1 7 1 1 1]
   [1 3 1 9 1 2 8 1 3 7]
   [1 3 5 9 9 1 2 4 2 1]
   [3 1 2 5 4 2 1 6 3 9]
   [1 2 9 3 1 3 8 5 2 1]
   [2 3 1 1 9 4 4 5 8 1]])

(defn part-1
  "What is the lowest total risk of any path
  from the top left to the bottom right?"
  {:test (examples part-1 [demo-cavern] 40)}
  [cavern]
  (let [width       (count (cavern 0))
        height      (count cavern)
        ; You start in the top left position
        start       [0 0]
        ; your destination is the bottom right position
        destination [(dec width) (dec height)]
        ; you cannot move diagonally
        neighbours  [[1 0] [0 -1] [-1 0] [0 1]]
        empty       (priority-map)]
    (loop [visited #{}
           ; don't count the risk level of your starting position
           queue   (priority-map start 0)]
      (let [[[x y :as position] risk] (peek queue)]
        (if (= destination position)
          risk
          (recur
            (conj visited position)
            (merge-with min
              (pop queue)
              (into empty
                (for [[dx dy] neighbours
                      :let [x (+ x dx)
                            y (+ y dy)]
                      :when (contains? cavern y)
                      :let [row (cavern y)]
                      :when (contains? row x)
                      :let [pos [x y]]
                      :when (not (contains? visited pos))]
                  [pos (+ risk (row x))])))))))))

(defn expand
  "Your original map tile repeats to the right and downward;
  each time the tile repeats to the right or downward,
  all of its risk levels are 1 higher
  than the tile immediately up or left of it."
  {:test (examples expand
           [[[8]] 5] [[8 9 1 2 3]
                      [9 1 2 3 4]
                      [1 2 3 4 5]
                      [2 3 4 5 6]
                      [3 4 5 6 7]]
           [[[1 4]
             [7 9]] 2] [[1 4 2 5]
                        [7 9 8 1]
                        [2 5 3 6]
                        [8 1 9 2]])}
  [cavern n]
  (let [width  (count (cavern 0))
        height (count cavern)]
    (vec
      (for [Y (range (*   n height))
            :let [y (mod  Y height)
                  b (quot Y height)
                  row (cavern y)]]
        (vec
          (for [X (range (*   n width))
                :let [x (mod  X width)
                      a (quot X width)]]
            (-> (row x)
              (+ a b)
              ; risk levels above 9 wrap back around to 1
              dec
              (mod 9)
              inc)))))))

(defn part-2
  "The entire cave is actually five times larger
  in both dimensions than you thought"
  {:test (examples part-2 [demo-cavern] 315)}
  [cavern]
  (part-1 (expand cavern 5)))

(run-tests)
