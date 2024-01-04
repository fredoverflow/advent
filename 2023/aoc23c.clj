(ns user
  (:require [clojure.string :as string]
            [clojure.test :refer [do-report run-tests]]))



(defn- filter-stack-trace! [^Throwable throwable]
  (->> (for [^StackTraceElement element (. throwable getStackTrace)
             :when (. *source-path* equals (. element getFileName))]
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



(defn re-seq-indexed [^java.util.regex.Pattern re s]
  (let [m (re-matcher re s)]
    ((fn step []
       (when (. m find)
         (cons [(. m start) (re-groups m)] (lazy-seq (step))))))))

(defn neighbours [index, ^String number, stride]
  (let [width  (.length number)
        width2 (+ 2 width)
        top    (dec (- index stride))
        bot    (dec (+ index stride))]
    (into
      [(dec index) (+ index width)]
      cat
      [(range top (+ top width2))
       (range bot (+ bot width2))])))

(defpure part1
  {["467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598.."]
   4361}
  "Any number adjacent to a symbol, even diagonally, is a 'part number'.
  Periods (.) do not count as a symbol.
  What is the sum of all of the part numbers in the engine schematic?"
  [^String schematic]
  (let [schematic (.replace schematic " " "")
        stride    (inc (.indexOf schematic "\n"))
        schematic (.replace schematic "\n" ".")
        padding   (.repeat "." (inc stride))
        schematic (str padding schematic padding)
        
        dot?      (fn [index] (= \. (.charAt schematic index)))]
    
    (reduce + 0
      (for [[index number] (re-seq-indexed #"\d+" schematic)
            :when (not-every? dot? (neighbours index number stride))]
        (parse-long number)))))



(defpure part2
  {["467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598.."]
   (+ (* 467 35) (* 755 598))}
  "A gear is any * symbol that is adjacent to exactly two part numbers.
  Its gear ratio is the result of multiplying those two numbers together.
  What is the sum of all of the gear ratios in your engine schematic?"
  [^String schematic]
  (let [schematic (.replace schematic " " "")
        stride    (inc (.indexOf schematic "\n"))
        schematic (.replace schematic "\n" ".")
        padding   (.repeat "." (inc stride))
        schematic (str padding schematic padding)
        
        gear?     (fn [index] (= \* (.charAt schematic index)))]
    
    (->> (for [[index number] (re-seq-indexed #"\d+" schematic)
               gear           (filter gear? (neighbours index number stride))]
           {gear [(parse-long number)]})
      (apply merge-with into)
      vals
      (filter second)
      (transduce (map (partial reduce * 1)) + 0))))



(run-tests)
