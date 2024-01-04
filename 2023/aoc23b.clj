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



(defpure max-rgb
  {["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"]
   {"red" 4, "green" 2, "blue" 6}
   
   ["Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"]
   {"red" 1, "green" 3, "blue" 4}
   
   ["Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"]
   {"red" 20, "green" 13, "blue" 6}
   
   ["Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"]
   {"red" 14, "green" 3, "blue" 15}
   
   ["Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"]
   {"red" 6, "green" 3, "blue" 2}}
  
  "What is the fewest number of cubes of each color
  that could have been in the bag to make the game possible?"
  [^String line]
  (apply merge-with max
    {"red"   0
     "green" 0
     "blue"  0}
    (for [[       _ count color]
          (re-seq #"(\d+) (\w+)" line)]
      {color (parse-long count)})))

(defn possible-rgb?
  "The bag contains only 12 red cubes, 13 green cubes, and 14 blue cubes."
  [^String line]
  (let [{:strs [red green blue]} (max-rgb line)]
    (and
      (<= red 12)
      (<= green 13)
      (<= blue 14))))

(defpure part1
  {["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
    Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
    Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
    Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"]
   (+ 1 2 5)}
  "Determine which games would have been possible if the bag had been loaded
  with only 12 red cubes, 13 green cubes, and 14 blue cubes.
  What is the sum of the IDs of those games?"
  [^String lines]
  (reduce + 0
    (for [line (string/split-lines lines)
          :when (possible-rgb? line)]
      (parse-long
        (re-find #"\d+" line)))))



(defpure part2
  {["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
    Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
    Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
    Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"]
   (+ 48 12 1560 630 36)}
  "The power of a set of cubes is equal to the numbers of
  red, green, and blue cubes multiplied together.
  What is the sum of the power of these sets?"
  [^String lines]
  (reduce + 0
    (for [line (string/split-lines lines)]
      (transduce (map val) * 1 (max-rgb line)))))



(run-tests)
