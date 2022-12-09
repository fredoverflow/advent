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



(def example
  "Each tree is represented as a single digit whose value is its height,
  where 0 is the shortest and 9 is the tallest."
  
  "30373
  25512
  65332
  33549
  35390")



(defpure string-to-digits
  {["1689023547"] [1 6 8 9 0 2 3 5 4 7]}
  [^String input]
  (mapv #(Character/digit % 10) input))



(defpure parse-grid
  {[example] [[3 0 3 7 3]
              [2 5 5 1 2]
              [6 5 3 3 2]
              [3 3 5 4 9]
              [3 5 3 9 0]]}
  [^String input]
  (mapv string-to-digits (string/split input #"\n *")))



(defpure transpose
  {[[[1 2 3]
     [4 5 6]]] [[1 4]
                [2 5]
                [3 6]]}
  [matrix]
  (apply mapv vector matrix))



(defpure visible?
  {[[5  3 3 2]] true
   [[5  3 5  ]] false
   [[5  6    ]] false
   [[5  5 0  ]] false}
  "A tree is visible if all of the other trees between it
  and an edge of the grid are shorter than it."
  [[tree & other-trees]]
  (every? #(< % tree) other-trees))



(defpure part1
  {[example] 21}
  "How many trees are visible from outside the grid?"
  [^String input]
  (let [grid   (parse-grid input)
        dirg   (transpose grid)
        length (count grid)
        width  (count (grid 0))]
    
    (count
      (for [y (range 0 length)
            x (range 0 width)
            :when (or
                    (visible?       (subvec (grid y) x         ))   ; right
                    (visible? (rseq (subvec (grid y) 0 (inc x))))   ; left
                    (visible?       (subvec (dirg x) y         ))   ; down
                    (visible? (rseq (subvec (dirg x) 0 (inc y)))))] ; up
        nil))))



(defpure viewing-distance
  {[[5  3    ]] 1
   [[5  5 2  ]] 1
   [[5  1 2  ]] 2
   [[5  3 5 3]] 2}
  "To measure the viewing distance from a given tree,
  stop if you reach an edge or at the first tree that is the same height
  or taller than the tree under consideration."
  [[tree & other-trees]]
  (let [smaller-trees (take-while #(< % tree) other-trees)]
    (min
      (count other-trees)            ; not blocked
      (inc (count smaller-trees))))) ; blocked by tree



(defpure part2
  {[example] 8}
  "What is the highest scenic score possible for any tree?
  A tree's scenic score is found by multiplying together
  its viewing distance in each of the four directions."
  [^String input]
  (let [grid   (parse-grid input)
        dirg   (transpose grid)
        length (count grid)
        width  (count (grid 0))]
    
    (reduce max 0
      (for [y (range 0 length)
            x (range 0 width)]
        (*
          (viewing-distance       (subvec (grid y) x         ))      ; right
          (viewing-distance (rseq (subvec (grid y) 0 (inc x))))      ; left
          (viewing-distance       (subvec (dirg x) y         ))      ; down
          (viewing-distance (rseq (subvec (dirg x) 0 (inc y))))))))) ; up



(run-tests)
