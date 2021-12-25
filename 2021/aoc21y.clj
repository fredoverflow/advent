(ns user
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is run-tests]]))

(defmacro examples [f & coll-arguments-result]
  `(fn []
     ~@(for [[arguments result] (partition 2 coll-arguments-result)]
         `(is (= ~(cons f arguments) ~result)))))

(def demo-region
  ["v...>>.vv>"
   ".vv>>.vv.."
   ">>.>v>...v"
   ">>v>>.>.v."
   "v>v.vv.v.."
   ">.>>..v..."
   ".vv..>.>v."
   "v.v..>>v.v"
   "....v..v.>"])

(defn move
  "Every step, the sea cucumbers in the east-facing herd attempt to move forward one location,
  then the sea cucumbers in the south-facing herd attempt to move forward one location.
  Every sea cucumber facing an empty location simultaneously moves into that location."
  {:test (examples move
           ([".........."
             ".>v....v.."
             ".......>.."
             ".........."]) [".........."
                             ".>........"
                             "..v....v>."
                             ".........."]
           (["...>..."
             "......."
             "......>"
             "v.....>"
             "......>"
             "......."
             "..vvv.."]) ["..vv>.."
                          "......."
                          ">......"
                          "v.....>"
                          ">......"
                          "......."
                          "....v.."])}
  [region]
  (let [height (count region)
        width  (count (region 0))
        
        yyy    (partition 3 1 (concat [(dec height)] (range height) [0]))
        xxx    (partition 3 1 (concat [(dec width )] (range width ) [0]))
        
        temp   (vec
                 (for [y (range height)
                       :let [row (region y)]]
                   (apply str
                     (for [[west x east] xxx]
                       (cond
                         (and (= \> (nth row west)) (= \. (nth row x   ))) \>
                         (and (= \> (nth row x   )) (= \. (nth row east))) \.
                         
                         :else (nth row x))))))]
    (vec
      (for [[north y south] yyy]
        (apply str
          (for [x (range width)]
            (cond
              (and (= \v (nth (temp north) x)) (= \. (nth (temp y    ) x))) \v
              (and (= \v (nth (temp y    ) x)) (= \. (nth (temp south) x))) \.
              
              :else (nth (temp y) x))))))))

(defn part-1
  "They'll eventually pile up and leave enough space.
  What is the first step on which no sea cucumbers move?"
  {:test (examples part-1 [demo-region] 58)}
  [region]
  (->> region
    (iterate move)
    (partition 2 1)
    (take-while (fn [[before after]] (not= before after)))
    count
    inc))

(run-tests)
