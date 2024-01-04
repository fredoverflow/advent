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



(defpure calibrate1
  {["1abc2"]       12
   ["pqr3stu8vwx"] 38
   ["a1b2c3d4e5f"] 15
   ["treb7uchet"]  77}
  
  "On each line, the calibration value can be found by combining
  the first digit and the last digit (in that order)
  to form a single two-digit number."
  [^String line]
  (let [digits (filter #(Character/isDigit %1) line)]
    (parse-long
      (str (first digits) (last digits)))))

(defpure part1
  {["1abc2
    pqr3stu8vwx
    a1b2c3d4e5f
    treb7uchet"] (+ 12 38 15 77)}
  
  "What is the sum of all of the calibration values?"
  [^String lines]
  (transduce (map calibrate1) + 0 (string/split-lines lines)))



(defpure calibrate2
  {["two1nine"]         29
   ["eightwothree"]     83
   ["abcone2threexyz"]  13
   ["xtwone3four"]      24
   ["4nineeightseven2"] 42
   ["zoneight234"]      14
   ["7pqrstsixteen"]    76}
  
  "It looks like some of the digits are actually spelled out with letters:
  one, two, three, four, five, six, seven, eight, and nine
  also count as valid 'digits'."
  [^String line]
  (->> (for [[       _      digit                                         ]
             (re-seq #"(?=(\d|one|two|three|four|five|six|seven|eight|nine))" line)]
         (case digit
           "one"   1
           "two"   2
           "three" 3
           "four"  4
           "five"  5
           "six"   6
           "seven" 7
           "eight" 8
           "nine"  9
           digit))
    
    ((juxt first last))
    string/join
    parse-long))

(defpure part2
  {["two1nine
    eightwothree
    abcone2threexyz
    xtwone3four
    4nineeightseven2
    zoneight234
    7pqrstsixteen"] (+ 29 83 13 24 42 14 76)}
  
  "What is the sum of all of the calibration values?"
  [^String lines]
  (transduce (map calibrate2) + 0 (string/split-lines lines)))



(run-tests)
