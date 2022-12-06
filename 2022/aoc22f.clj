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



(defpure part1
  {;    ____
   ["mjqjpqmgbljsphdztnvjfqwrcgsmlb"]     7
   ;  ____
   ["bvwbjplbgvbhsrlpgdmjqwftvncz"]       5
   ;   ____
   ["nppdvjthqldpwncqszvftbrmjlhg"]       6
   ;       ____
   ["nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"] 10
   ;        ____
   ["zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"]  11}
  
  "The start of a packet is indicated by a sequence of four characters
  that are all different. How many characters need to be processed
  before the first start-of-packet marker is detected?"
  [^String input]
  (loop [j 4]
    (let [window (. input substring (- j 4) j)]
      (if (apply distinct? window)
        j
        (recur (inc j))))))



(defpure part2
  {;      ______________
   ["mjqjpqmgbljsphdztnvjfqwrcgsmlb"]    19
   ;          ______________
   ["bvwbjplbgvbhsrlpgdmjqwftvncz"]      23
   ;          ______________
   ["nppdvjthqldpwncqszvftbrmjlhg"]      23
   ;                ______________
   ["nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"] 29
   ;             ______________
   ["zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"]  26}
  
  "A start-of-message marker consists of 14 distinct characters.
  How many characters need to be processed
  before the first start-of-message marker is detected?"
  [^String input]
  (first
    (for [j (range 14 (count input))
          :let [window (. input substring (- j 14) j)]
          :when (apply distinct? window)]
      j)))



(run-tests)
