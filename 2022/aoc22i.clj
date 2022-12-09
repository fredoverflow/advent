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
  "This series of motions moves the head right four steps,
  then up four steps,
  then left three steps,
  then down one step, and so on."
  
  "R 4
  U 4
  L 3
  D 1
  R 4
  D 1
  L 5
  R 2")



(defpure unroll-directions
  {[example] [[+1  0] [+1  0] [+1  0] [+1  0]
              [ 0 -1] [ 0 -1] [ 0 -1] [ 0 -1]
              [-1  0] [-1  0] [-1  0]
              [ 0 +1]
              [+1  0] [+1  0] [+1  0] [+1  0]
              [ 0 +1]
              [-1  0] [-1  0] [-1  0] [-1  0] [-1  0]
              [+1  0] [+1  0]]}
  [^String input]
  (for [[_ lrud n] (re-seq #"(L|R|U|D) (\d+)" input)
        
        direction (repeat (parse-long n) ({\L [-1  0]
                                           \R [+1  0]
                                           \U [ 0 -1]
                                           \D [ 0 +1]} (nth lrud 0)))]
    direction))



(defn plus [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn minus [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])



(def forces1
  "If the head is ever two steps directly up, down, left, or right from the tail,
  the tail must also move one step in that direction so it remains close enough.
  
  If the head and tail aren't touching and aren't in the same row or column,
  the tail always moves one step diagonally to keep up."
  
  {[-2  0] [-1  0]
   [+2  0] [+1  0]
   [ 0 -2] [ 0 -1]
   [ 0 +2] [ 0 +1]
   
   [-2 -1] [-1 -1]
   [-2 +1] [-1 +1]
   [+2 -1] [+1 -1]
   [+2 +1] [+1 +1]
   [-1 -2] [-1 -1]
   [+1 -2] [+1 -1]
   [-1 +2] [-1 +1]
   [+1 +2] [+1 +1]})



(defpure part1
  {[example] 13}
  "How many positions does the tail of the rope visit at least once?"
  [^String input]
  (loop [head [0 0]
         tail head
         visited #{}
         [direction & directions] (unroll-directions input)]
    
    (let [visited (conj visited tail)]
      (if-not direction
        (count visited)
        (let [head  (plus head direction)
              delta (minus head tail)
              force (forces1 delta)
              tail  (if force (plus tail force) tail)]
          (recur head tail visited directions))))))



(def forces2
  "More types of motion are possible than before."
  
  (assoc forces1
    [-2 -2] [-1 -1]
    [-2 +2] [-1 +1]
    [+2 -2] [+1 -1]
    [+2 +2] [+1 +1]))



(defn follow
  "Each knot further down the rope follows the knot in front of it
  using the same rules as before."
  ; A BCDE
  ; AB CDE
  ; ABC DE
  ; ABCD E
  ; ABCDE 
  [[moved-head first-tail & more-tails :as rope]]
  (if-not first-tail
    rope
    (let [delta (minus moved-head first-tail)
          force (forces2 delta)]
      (if-not force
        rope
        (cons moved-head (follow (cons (plus first-tail force) more-tails)))))))



(defpure part2
  {[example] 1 
   ["R 5
    U 8
    L 8
    D 3
    R 17
    D 10
    L 25
    U 20"] 36}
  
  "Rather than two knots, you now must simulate a rope consisting of ten knots."
  [^String input]
  (loop [[head & tails] (repeat 10 [0 0])
         visited #{}
         [direction & directions] (unroll-directions input)]
    
    (let [visited (conj visited (last tails))]
      (if-not direction
        (count visited)
        (let [head (plus head direction)]
          (recur (follow (cons head tails)) visited directions))))))



(run-tests)
