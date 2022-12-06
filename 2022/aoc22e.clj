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
 "    [D]    
  [N] [C]    
  [Z] [M] [P]
  _1   2   3 
  
  move 1 from 2 to 1
  move 3 from 1 to 3
  move 2 from 2 to 1
  move 1 from 1 to 2")



(defpure parse-stacks
  {[example] ['()
              '(\N \Z)
              '(\D \C \M)
              '(\P)]}
  [^String input]
  (let [lines (string/split input #"\n *")
        crates (take-while seq lines)
        columns (apply mapv str crates)]
    (into [()]
      (for [column columns
            :let [letters (re-find #"[A-Z]+" column)]
            :when letters]
        (apply list letters)))))



(defpure parse-moves
  {[example] [[1 2 1]
              [3 1 3]
              [2 2 1]
              [1 1 2]]}
  [^String input]
  (for [[_ & n-from-to] (re-seq #"move (\d+) from (\d+) to (\d+)" input)]
    (mapv parse-long n-from-to)))



(defpure move1
  {[['(\A \B \C)
     '(\X)] [2 0 1]] ['(\C)
                      '(\B \A \X)]}
  "Crates are moved one at a time,
  so the first crate to be moved ends up below the second"
  [stacks [n from to]]
  (loop [source (stacks from)
         target (stacks to)
         n      n]
    (if (pos? n)
      (recur
        (pop source)
        (conj target (peek source))
        (dec n))
      (assoc stacks
        from source
        to   target))))



(defpure part1
  {[example] "CMZ"}
  "After the rearrangement procedure completes,
  what crate ends up on top of each stack?"
  [input]
  (->> (reduce move1 (parse-stacks input) (parse-moves input))
    (map peek)
    (apply str)))



(defpure move2
  {[['(\A \B \C)
     '(\X)] [2 0 1]] ['(\C)
                      '(\A \B \X)]}
  "pick up and move multiple crates at once"
  [stacks [n from to]]
  (loop [source (stacks from)
         temp   ()
         n      n]
    (if (pos? n)
      (recur
        (pop source)
        (conj temp (peek source))
        (dec n))
      (assoc stacks
        from source
        to   (into (stacks to) temp)))))



(defpure part2
  {[example] "MCD"}
  "After the rearrangement procedure completes,
  what crate ends up on top of each stack?"
  [input]
  (->> (reduce move2 (parse-stacks input) (parse-moves input))
    (map peek)
    (apply str)))



(run-tests)
