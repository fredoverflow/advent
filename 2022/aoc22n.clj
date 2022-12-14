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
  ;   4     5  5
  ;   9     0  0
  ;   4     0  3
  ; 0 ......+...
  ; 1 ..........
  ; 2 ..........
  ; 3 ..........
  ; 4 ....#...##
  ; 5 ....#...#.
  ; 6 ..###...#.
  ; 7 ........#.
  ; 8 ........#.
  ; 9 #########.
  
  "498,4 -> 498,6 -> 496,6
  503,4 -> 502,4 -> 502,9 -> 494,9")



(defpure parse-paths
  {[example] [[[498 4] [498 6] [496 6]]
              [[503 4] [502 4] [502 9] [494 9]]]}
  [^String input]
  (for [path (string/split input #"\n *")]
    (for [[_ & xy] (re-seq #"(\d+),(\d+)" path)]
      (mapv parse-long xy))))



(defn draw-line [^booleans grid, [[x1 y1] [x2 y2]]]
  (let [dx (Integer/signum (- x2 x1))  ; -1 0 +1
        dy (Integer/signum (- y2 y1))] ; -1 0 +1
    
    (loop [x x1
           y y1]
      (aset-boolean grid (+ x (* y 1000)) true)
      
      (if-not (and (= x x2) (= y y2))
        (recur (+ x dx) (+ y dy))))))



(defn make-grid ^booleans [paths]
  (let [grid (boolean-array 1000000)]
    (doseq [path paths
            line (partition 2 1 path)]
      (draw-line grid line))
    grid))



(defn drop-grain
  "Attemps to drop another grain onto the grid.
  Returns false if it fell into the abyss below."
  ^Boolean [^booleans grid]
  (loop [grain 500]
    (let [down  (+ grain 1000)
          left  (dec down)
          right (inc down)]
      (cond
        (>= down 1000000)          false
        ;                                        ;    +
        (false? (aget grid down))  (recur down)  ;    .
        ;                                        ;  +
        (false? (aget grid left))  (recur left)  ; .o
        ;                                        ;      +
        (false? (aget grid right)) (recur right) ;     oo.
        ;                                        ;  +
        :else (aset-boolean grid grain true))))) ; ooo



(defpure part1
  {[example] 24}
  "How many units of sand come to rest
  before sand starts flowing into the abyss below?"
  [^String input]
  (let [paths (parse-paths input)
        grid  (make-grid paths)]
    
    (loop [counter 0]
      (if (drop-grain grid)
        (recur (inc counter))
        counter))))



(defpure find-floor
  {[(parse-paths example)] [[0 11] [999 11]]}
  "The floor is an infinite horizontal line with a y coordinate equal to
  two plus the highest y coordinate of any point in your scan."
  [paths]
  (let [y (->> (for [path  paths
                     [x y] path]
                 y)
            (reduce max 0)
            (+ 2))]
    
    [[0 y] [999 y]]))



(defpure part2 ; 30 seconds for my input
  {[example] 93}
  "Simulate the falling sand until the source of the sand becomes blocked.
  How many units of sand come to rest?"
  [^String input]
  (let [paths (parse-paths input)
        floor (find-floor paths)
        grid  (make-grid (cons floor paths))]
    
    (loop [counter 0]
      (if (aget grid 500)
        counter
        (do
          (drop-grain grid)
          (recur (inc counter)))))))



; ............o............
; ...........ooo...........
; ..........ooooo..........
; .........ooooooo.........
; ........oo#ooo##o........
; .......ooo#ooo#ooo.......
; ......oo###ooo#oooo......
; .....oooo.oooo#ooooo.....
; ....oooooooooo#oooooo....
; ...ooo#########ooooooo...
; ..ooooo.......ooooooooo..
; #########################



(defpure part2bfs ; 200 milliseconds for my input
  {[example] 93}
  "Simulate the falling sand until the source of the sand becomes blocked.
  How many units of sand come to rest?"
  [^String input]
  (let [paths (parse-paths input)
        floor (find-floor paths)
        grid  (make-grid (cons floor paths))]
    
    (loop [counter 0
           grains  [500]] ; grains in the currently explored line
      
      (let [n (count grains)]
        (if (zero? n)
          counter
          (recur
            (+ counter n)
            (into []
              (comp
                (mapcat (fn [grain] [(+ grain  999)    ; down left
                                     (+ grain 1000)    ; down
                                     (+ grain 1001)])) ; down right
                (distinct)
                (remove (fn [grain] (aget grid grain))))
              grains)))))))



(run-tests)
