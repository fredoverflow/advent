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
  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
  Sensor at x=9, y=16: closest beacon is at x=10, y=16
  Sensor at x=13, y=2: closest beacon is at x=15, y=3
  Sensor at x=12, y=14: closest beacon is at x=10, y=16
  Sensor at x=10, y=20: closest beacon is at x=10, y=16
  Sensor at x=14, y=17: closest beacon is at x=10, y=16
  Sensor at x=8, y=7: closest beacon is at x=2, y=10
  Sensor at x=2, y=0: closest beacon is at x=2, y=10
  Sensor at x=0, y=11: closest beacon is at x=2, y=10
  Sensor at x=20, y=14: closest beacon is at x=25, y=17
  Sensor at x=17, y=20: closest beacon is at x=21, y=22
  Sensor at x=16, y=7: closest beacon is at x=15, y=3
  Sensor at x=14, y=3: closest beacon is at x=15, y=3
  Sensor at x=20, y=1: closest beacon is at x=15, y=3")

; ..........#.................
; .........###................
; ....S...#####...............
; .......#######........S.....
; ......#########S............
; .....###########SB..........
; ....#############...........
; ...###############..........
; ..#################.........
; .#########S#######S#........
; ..#################.........
; ...###############..........
; ....B############...........
; ..S..###########............
; ......#########.............
; .......#######..............
; ........#####.S.......S.....
; B........###................
; ..........#SB...............

(defpure sensors-beacons-manhattens
  {[example] [[[ 2 18] [-2 15]  7]
              [[ 9 16] [10 16]  1]
              [[13  2] [15  3]  3]
              [[12 14] [10 16]  4]
              [[10 20] [10 16]  4]
              [[14 17] [10 16]  5]
              [[ 8  7] [ 2 10]  9]
              [[ 2  0] [ 2 10] 10]
              [[ 0 11] [ 2 10]  3]
              [[20 14] [25 17]  8]
              [[17 20] [21 22]  6]
              [[16  7] [15  3]  5]
              [[14  3] [15  3]  1]
              [[20  1] [15  3]  7]]}
  [^String input]
  (for [[_ & sx-sy-bx-by] (re-seq #"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)" input)
        
        :let [[sx sy bx by] (mapv parse-long sx-sy-bx-by)
              
              manhatten     (+
                              (abs (- bx sx))
                              (abs (- by sy)))]]
    
    [[sx sy] [bx by] manhatten]))



(defn make-slices
  [senbeamans, ^long row]
  (for [[[sx sy] _ manhatten] senbeamans
        
        :let [dy (abs (- row sy))
              dx (- manhatten dy)]
        
        :when (not (neg? dx))
        
        :let [left  (- sx dx)
              right (+ sx dx)]]
    
    [left (inc right)]))



(defpure merge-slices
  {[[[10 20]]] [[10 20]]
   [[[10 20] [30 40]]] [[10 20] [30 40]]
   [[[10 20] [30 40] [50 60]]] [[10 20] [30 40] [50 60]]
   
   [[[10 20] [20 30]]] [[10 30]]
   [[[10 20] [15 25]]] [[10 25]]
   [[[10 20] [12 18]]] [[10 20]]
   
   ; Part 1
   [[[-2 3] [2 3] [2 15] [12 13] [14 19] [16 25]]] [[-2 25]]
   
   ; Part2
   [[[-3 4] [2 3] [3 14] [11 14]  [15 18] [15 26]]] [[-3 14] [15 26]]}
  [slices]
  (loop [result (transient [])
         
         [[left1 right1 :as one]
          [left2 right2 :as two] & slices] (sort slices)]
    (cond
      (nil? two)        (persistent! (conj! result one))
      
      (<= left2 right1) (recur result (cons [left1 (max right1 right2)] slices))
      
      :otherwise        (recur (conj! result one) (cons two slices)))))



(defpure part1
  {[example 10] 26}
  "In the row, how many positions cannot contain a beacon?"
  [^String input, ^long row]
  (let [senbeamans (sensors-beacons-manhattens input)
        
        slices     (merge-slices (make-slices senbeamans row))
        
        beacon-xs  (into #{}
                     (keep
                       (fn [[_ [bx by]]]
                         (if (= row by)
                           bx)))
                     senbeamans)]
    
    (transduce
      (map (fn [[left right]]
             (- right
               left
               (count (filterv
                        (fn [x]
                          (and
                            (<= left x)
                            (< x right)))
                        beacon-xs)))))
      +
      0
      slices)))



(defpure part2
  {[example 20] 56000011}
  "Find the only possible position for the distress beacon.
  Multiply its x coordinate by 4000000 and then add its y coordinate."
  [^String input, ^long bound]
  (let [senbeamans (sensors-beacons-manhattens input)]
    (loop [y 0]
      (let [slices (merge-slices (make-slices senbeamans y))]
        (if (second slices)
          (let [[[_ x]] slices]
            (+ y (* x 4000000)))
          (if (< y bound) (recur (inc y))))))))



(run-tests)
