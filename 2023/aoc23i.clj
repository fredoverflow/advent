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



(defpure parse-longs
  {["0 3 6 9 12 -15"] [0 3 6 9 12 -15]}
  "Each line in the report contains the history of a single value."
  [^String line]
  (->> line
    (re-seq #"-?\d+")
    (mapv parse-long)))

(defpure differences
  {[[0 3 6 9 12 15]]
   [  3 3 3 3  3   ]
   
   [[3 3 3 3 3]]
   [  0 0 0 0  ]}
  "a new sequence from the difference at each step of your history"
  [history]
  (->> history
    (partition 2 1)
    (mapv (fn [[a b]] (- b a)))))

(defpure next-value
  {[[ 0  3  6  9 12 15]] 18
   [[ 1  3  6 10 15 21]] 28
   [[10 13 16 21 30 45]] 68}
  "While that sequence is not all zeroes, repeat this process,
  using the sequence you just generated as the input sequence."
  [history]
  (->> history
    (iterate differences)
    (take-while #(not (every? zero? %)))
    (transduce (map peek) + 0)))

(defpure part1
  {["0 3 6 9 12 15
    1 3 6 10 15 21
    10 13 16 21 30 45"]
   114}
  "Extrapolate the next value for each history.
  What is the sum of these extrapolated values?"
  [^String lines]
  (->> lines
    string/split-lines
    (transduce
      (comp
        (map parse-longs)
        (map next-value))
      + 0)))



(run-tests)



(defn part2
  "Extrapolate the previous value for each history.
  What is the sum of these extrapolated values?"
  [^String lines]
  (->> lines
    string/split-lines
    (transduce
      (comp
        (map parse-longs)
        (map rseq)
        (map vec)
        (map next-value))
      + 0)))
