(ns user
  (:require [clojure.string :as string]
            [clojure.set :as set]
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



(defpure split-lines
  {["first line
    second line
    third line"] ["first line"
                  "second line"
                  "third line"]}
  
  "Splits s into lines. Trims leading space."
  [^String s]
  (string/split s #"\n *"))



(defpure cut-in-half
  {["abcdef"] ["abc" "def"]} 
  [^String s]
  (let [length (count s)
        middle (quot length 2)
        front  (. s substring 0      middle)
        back   (. s substring middle length)]
    [front back]))



(defpure common-letter
  {; Part 1
   [["vJrwpWtwJgWr" "hcsFMMfFFhFp"]] \p
   ; Part 2
   [["vJrwpWtwJgWrhcsFMMfFFhFp"
     "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
     "PmmdzqPrVvPwwTWBwg"]]          \r}
  
  "Which letter occurs in all strings?"
  [strings]
  (->> (map set strings)
    (apply set/intersection)
    first))



; Lowercase item types a through z have priorities  1 through 26.
; Uppercase item types A through Z have priorities 27 through 52.

(def priority (zipmap
                "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                (range 1 53)))



(defpure part1
  {["vJrwpWtwJgWrhcsFMMfFFhFp
    jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
    PmmdzqPrVvPwwTWBwg
    wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
    ttgJtRGJQctTZtZT
    CrZsJsPPZsGzwwsLwLmpwMDw"] 157}
  
  "Find the item type that appears in both compartments of each rucksack.
  What is the sum of the priorities of those item types?"
  [^String rucksacks]
  (transduce
    (comp
      (map cut-in-half)
      (map common-letter)
      (map priority))
    + 0
    (split-lines rucksacks)))



(defpure part2
  {["vJrwpWtwJgWrhcsFMMfFFhFp
    jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
    PmmdzqPrVvPwwTWBwg
    wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
    ttgJtRGJQctTZtZT
    CrZsJsPPZsGzwwsLwLmpwMDw"] 70}
  
  "Find the item type that is common between all three Elves in each group.
  What is the sum of the priorities of those item types?"
  [^String rucksacks]
  (transduce
    (comp
      (partition-all 3)
      (map common-letter)
      (map priority))
    + 0
    (split-lines rucksacks)))



(run-tests)
