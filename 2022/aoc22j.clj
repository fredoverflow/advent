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
  "The CPU uses these instructions in a program
  to, somehow, tell the screen what to draw."
  
  "addx 15
  addx -11
  addx 6
  addx -3
  addx 5
  addx -1
  addx -8
  addx 13
  addx 4
  noop
  addx -1
  addx 5
  addx -1
  addx 5
  addx -1
  addx 5
  addx -1
  addx 5
  addx -1
  addx -35
  addx 1
  addx 24
  addx -19
  addx 1
  addx 16
  addx -11
  noop
  noop
  addx 21
  addx -15
  noop
  noop
  addx -3
  addx 9
  addx 1
  addx -3
  addx 8
  addx 1
  addx 5
  noop
  noop
  noop
  noop
  noop
  addx -36
  noop
  addx 1
  addx 7
  noop
  noop
  noop
  addx 2
  addx 6
  noop
  noop
  noop
  noop
  noop
  addx 1
  noop
  noop
  addx 7
  addx 1
  noop
  addx -13
  addx 13
  addx 7
  noop
  addx 1
  addx -33
  noop
  noop
  noop
  addx 2
  noop
  noop
  noop
  addx 8
  noop
  addx -1
  addx 2
  addx 1
  noop
  addx 17
  addx -9
  addx 1
  addx 1
  addx -3
  addx 11
  noop
  noop
  addx 1
  noop
  addx 1
  noop
  noop
  addx -13
  addx -19
  addx 1
  addx 3
  addx 26
  addx -30
  addx 12
  addx -1
  addx 3
  addx 1
  noop
  noop
  noop
  addx -9
  addx 18
  addx 1
  addx 2
  noop
  noop
  addx 9
  noop
  noop
  noop
  addx -1
  addx 2
  addx -37
  addx 1
  addx 3
  noop
  addx 15
  addx -21
  addx 22
  addx -6
  addx 1
  noop
  addx 2
  addx 1
  noop
  addx -10
  noop
  noop
  addx 20
  addx 1
  addx 2
  addx 2
  addx -6
  addx -11
  noop
  noop
  noop")



(defpure parse-instructions
  {["noop
    addx 3
    addx -5"] [["noop" nil]
               ["addx"   3]
               ["addx"  -5]]}
  [^String input]
  (for [[_  instruction _ argument] (re-seq #"(\w+)( (-?\d+))?" input)]
    [instruction (some-> argument parse-long)]))



(defpure X-register-values
  {["noop
    addx 3
    addx -5"] [1
               1 1
               4 4
               -1]}
  [^String input]
  (loop [result (transient [])
         X 1
         [[instruction argument] & instructions] (parse-instructions input)]
    
    (if-not instruction
      (persistent! (conj! result X))
      (case instruction
        
        "noop" (recur
                 (conj! result X)
                 X
                 instructions)
        
        "addx" (recur
                 (reduce conj! result (repeat 2 X))
                 (+ X argument)
                 instructions)))))



(defpure part1
  {[example] 13140}
  "Consider the signal strength
  (the cycle number multiplied by the value of the X register)
  during the 20th cycle and every 40 cycles after that.
  
  What is the sum of these six signal strengths?"
  [^String input]
  (transduce
    (comp
      (drop 19)
      (take-nth 40))
    +
    0
    (map * (range 1 Long/MAX_VALUE) (X-register-values input))))



(defn screen-pixel
  "If the sprite is positioned such that one of its three pixels
  is the pixel currently being drawn, the screen produces a lit pixel (#);
  otherwise, the screen leaves the pixel dark (.)"
  [crt-position sprite-middle]
  (case (- crt-position sprite-middle)
    (-1 0 +1) \#
    \.))



(defpure part2
  {[example] ["##..##..##..##..##..##..##..##..##..##.."
              "###...###...###...###...###...###...###."
              "####....####....####....####....####...."
              "#####.....#####.....#####.....#####....."
              "######......######......######......####"
              "#######.......#######.......#######....."]}
  
  "The X register controls the horizontal position of a sprite.
  Specifically, the sprite is 3 pixels wide, and the X register
  sets the horizontal position of the MIDDLE of that sprite.
  
  If the sprite's horizontal position puts its pixels where the CRT
  is currently drawing, then those pixels will be drawn.
  
  Render the image given by your program.
  What eight capital letters appear on your CRT?"
  [^String input]
  (->> (map screen-pixel (cycle (range 0 40)) (X-register-values input))
    (partition 40)
    (map string/join)))



(run-tests)
