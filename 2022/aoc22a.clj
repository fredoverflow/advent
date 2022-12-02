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



(def example "1000
  2000
  3000
  
  4000
  
  5000
  6000
  
  7000
  8000
  9000
  
  10000")



(defpure paragraph-lines
  {[example] [["1000" "2000" "3000"]
              ["4000"]
              ["5000" "6000"]
              ["7000" "8000" "9000"]
              ["10000"]]}
  "Splits s into paragraphs, and paragraphs into lines. Trims leading space."
  [^String s]
  (for [paragraph (string/split s #"\n *\n *")]
    (string/split paragraph #"\n *")))



(defpure part1
  {[example] 24000}
  "Find the Elf carrying the most Calories.
  How many total Calories is that Elf carrying?"
  [^String all-calories]
  (->> (for [elf-calories (paragraph-lines all-calories)]
         (transduce (map parse-long) + 0 elf-calories))
    (apply max)))



(defpure part2
  {[example] 45000}
  "Find the top three Elves carrying the most Calories.
  How many Calories are those Elves carrying in total?"
  [^String all-calories]
  (->> (for [elf-calories (paragraph-lines all-calories)]
         (transduce (map parse-long) + 0 elf-calories))
    (sort >)
    (transduce (take 3) + 0)))



(run-tests)
