(ns user
  (:require [clojure.string :as string]
            [clojure.zip :as zip]
            [clojure.test :refer [is run-tests]]))

(defmacro examples [f & coll-arguments-result]
  `(fn []
     ~@(for [[arguments result] (partition 2 coll-arguments-result)]
         `(is (= ~(cons f arguments) ~result)))))

(def demo-homework
  [[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
   [[[5,[2,8]],4],[5,[[9,9],0]]]
   [6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
   [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
   [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
   [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
   [[[[5,4],[7,7]],8],[[8,3],8]]
   [[9,3],[[9,9],[6,[4,9]]]]
   [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
   [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]])

(defn prev-child
  "Zipper location of previous child, or nil if none exists"
  [z]
  (loop [z (zip/prev z)]
    (cond
      (nil?        z) nil
      (zip/branch? z) (recur (zip/prev z))
      :else           z)))

(defn next-child
  "Zipper location of next child, or z if none exists"
  [z]
  (loop [z (zip/next z)]
    (cond
      (zip/end?    z) z
      (zip/branch? z) (recur (zip/next z))
      :else           z)))

(defn split
  "Splits the snailfish number once if possible, otherwise returns nil
  a   11   z
  a [5, 6] z"
  {:test (examples split
           ([[[[0,7],4],[15,[0,13]]],[1,1]])    [[[[0,7],4],[[7,8],[0,13]]],[1,1]]
           ([[[[0,7],4],[[7,8],[0,13]]],[1,1]]) [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]])}
  [snail]
  (loop [z (-> snail zip/vector-zip next-child)]
    (cond
      (zip/end? z)             nil
      (-> (zip/node z) (< 10)) (recur (next-child z))
      ; If any regular number is 10 or greater,
      ; the leftmost such regular number splits
      :else (-> z
              ; To split a regular number, replace it with a pair
              (zip/edit (fn [n]
                          ; divided by two and rounded down
                          [(quot n 2)
                           ; divided by two and rounded up
                           (quot (inc n) 2)]))
              zip/root))))

(defn explode
  "Explodes the snailfish number once if possible, otherwise returns nil
  [[ a [[ [l,r] ]] z ]]
  [[a+l[[   0   ]]r+z]]"
  {:test (examples explode
           ([[[[[9,8],1],2],3],4]) [[[[0,9],2],3],4]
           ([7,[6,[5,[4,[3,2]]]]]) [7,[6,[5,[7,0]]]]
           ([[6,[5,[4,[3,2]]]],1]) [[6,[5,[7,0]]],3]
           ([[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]) [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]
           ([[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]])     [[3,[2,[8,0]]],[9,[5,[7,0]]]])}
  [snail]
  (loop [z (-> [0   snail   0] zip/vector-zip next-child)]
    (cond     ; ^ sentinels ^
      (zip/end? z)                  nil
      (-> (zip/path z) count (< 6)) (recur (next-child z))
      ; If any pair is nested inside four pairs,
      ; the leftmost such pair explodes
      :else (let [left  (zip/node z)
                  right (zip/node (zip/right z))]
              (-> z
                ; exploding pair is replaced with the regular number 0
                zip/up
                (zip/replace 0)
                ; left is added to the first regular number to the left
                prev-child
                (zip/edit + left)
                next-child
                ; right is added to the first regular number to the right
                next-child
                (zip/edit + right)
                zip/root
                second)))))

(defn add
  "Adds two snailfish numbers and reduces the result"
  {:test (examples add
           ([[[[4,3],4],4],[7,[[8,4],9]]] [1,1]) [[[[0,7],4],[[7,8],[6,0]]],[8,1]])}
  [left right]
  ; Form a pair from the left and right parameters
  (loop [snail [left right]]
    ; Repeatedly do the first action in this list that applies:
    ; 1. If any pair is nested inside four pairs,
    ;    the leftmost such pair explodes
    (if-let [snail (explode snail)]
      (recur snail)
      ; 2. If any regular number is 10 or greater,
      ;    the leftmost such regular number splits
      (if-let [snail (split snail)]
        (recur snail)
        ; Once no action in the above list applies,
        ; the snailfish number is reduced
        snail))))

(defn magnitude
  "The snailfish teacher only checks the magnitude of the final sum"
  {:test (examples magnitude
           ([9,1])              29
           ([[9,1],[1,9]])     129
           ([[1,2],[[3,4],5]]) 143
           ([[[[0,7],4],[[7,8],[6,0]]],[8,1]]) 1384
           ([[[[1,1],[2,2]],[3,3]],[4,4]])      445
           ([[[[3,0],[5,3]],[4,4]],[5,5]])      791
           ([[[[5,0],[7,4]],[5,5]],[6,6]])     1137
           ([[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]) 3488)}
  [snail]
  (if (number? snail)
    ; The magnitude of a regular number is just that number
    snail
    (let [[left right] snail]
      ; 3 times the magnitude of its left element
      ; plus 2 times the magnitude of its right element
      (+
        (* 3 (magnitude left))
        (* 2 (magnitude right))))))

(defn part-1
  "What is the magnitude of the final sum?"
  {:test (examples part-1 (demo-homework) 4140)}
  [homework]
  (magnitude (reduce add homework)))

(defn part-2
  "What is the largest magnitude of any sum of two different snailfish numbers?"
  {:test (examples part-2 (demo-homework) 3993)}
  [homework]
  (apply max
    (for [left  homework
          right homework
          :when (not (identical? left right))]
      (magnitude (add left right)))))

(run-tests)
