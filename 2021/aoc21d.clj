(ns user
  (:require [clojure.string :as string]
            [clojure.test :refer [is run-tests]]))

(defmacro examples [f & coll-arguments-result]
  `(fn []
     ~@(for [[arguments result] (partition 2 coll-arguments-result)]
         `(is (= ~(cons f arguments) ~result)))))

(def demo-numbers [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1])

(def demo-boards
  (->>
    [22 13 17 11  0
      8  2 23  4 24
     21  9 14 16  7
      6 10  3 18  5
      1 12 20 15 19
     
      3 15  0  2 22
      9 18 13 17  5
     19  8  7 25 23
     20 11 10 24  4
     14 21 16 12  6
     
     14 21 17 24  4
     10 16 15  9 19
     18  8 23 26 20
     22 11 13  6  5
      2  0 12  3  7]
    
    (partition 25)
    (mapv vec)))

(defn parse-board
  {:test (examples parse-board
           [[5 0 6
             1 4 8
             3 2 7]] {:positions {5 0, 0 1, 6 2, 1 3, 4 4, 8 5, 3 6, 2 7, 7 8},
                      :rows [3 3 3], :cols [3 3 3]})}
  [board]
  (let [square    (count board)
        dimension (int (Math/sqrt square))
        unmarked  (vec (repeat dimension dimension))]
    {:positions (into {}
                  (map-indexed
                    (fn [index item] [item index]))
                  board)
     :rows unmarked, :cols unmarked}))

(defn mark
  {:test (examples mark
           [{:positions {5 0, 0 1, 6 2, 1 3, 4 4, 8 5, 3 6, 2 7, 7 8},
             :rows [3 3 3], :cols [3 3 3]}
            2]
           {:positions {5 0, 0 1, 6 2, 1 3, 4 4, 8 5, 3 6, 7 8},
            :rows [3 3 2], :cols [3 2 3]})}
  [board number]
  (if-let [position ((board :positions) number)]
    (let [dimension (count (board :rows))
          row       (quot position dimension)
          col       (rem position dimension)]
      (-> board
        (update :positions dissoc number)
        (update-in [:rows row] dec)
        (update-in [:cols col] dec)))
    board))

(defn winning? [board]
  (or
    (. (board :rows) contains 0)
    (. (board :cols) contains 0)))

(defn unmarked-sum [board]
  (->> board
    :positions
    keys
    (reduce + 0)))

(defn part-1
  {:test (examples part-1 [demo-boards demo-numbers] 4512)}
  [boards numbers]
  (loop [boards             (mapv parse-board boards)
         [number & numbers] numbers]
    (let [boards         (mapv #(mark %1 number) boards)
          winning-boards (filterv winning? boards)]
      (if (empty? winning-boards)
        (recur boards numbers)
        (* number (unmarked-sum (winning-boards 0)))))))

(defn part-2
  {:test (examples part-2 [demo-boards demo-numbers] 1924)}
  [boards numbers]
  (loop [boards             (mapv parse-board boards)
         [number & numbers] numbers]
    (let [boards                                      (mapv #(mark %1 number) boards)
          {winning-boards true, playing-boards false} (group-by winning? boards)]
      (if (empty? playing-boards)
        (* number (unmarked-sum (winning-boards 0)))
        (recur playing-boards numbers)))))

(run-tests)
