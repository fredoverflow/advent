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
  "The Elves pair up and make a big list
  of the section assignments for each pair."
  
  "2-4,6-8
  2-3,4-5
  5-7,7-9
  2-8,3-7
  6-6,4-6
  2-6,4-8") ; 23456,45678

(defpure parse-pairs
  {[example] [[2 4 6 8]
              [2 3 4 5]
              [5 7 7 9]
              [2 8 3 7]
              [6 6 4 6]
              [2 6 4 8]]}
  [^String input]
  (for [[_ & abxy] (re-seq #"(\d+)-(\d+),(\d+)-(\d+)" input)]
    (mapv parse-long abxy)))



(defpure fully-contains?
  {[[2 4 6 8]] false
   [[2 3 4 5]] false
   [[5 7 7 9]] false
   [[2 8 3 7]] true
   [[6 6 4 6]] true
   [[2 6 4 8]] false}
  "Some of the pairs have noticed that
  one of their assignments fully contains the other."
  [[a b x y]]
  (or
    (<= a x y b)
    (<= x a b y)))



(defpure part1
  {[example] 2}
  "In how many assignment pairs does one range fully contain the other?"
  [^String input]
  (count (filter fully-contains? (parse-pairs input))))



(defpure separate?
  {[[2 4 6 8]] true
   [[2 3 4 5]] true
   [[5 7 7 9]] false
   [[2 8 3 7]] false
   [[6 6 4 6]] false
   [[2 6 4 8]] false}
  "The Elves would like to know the number of pairs that overlap at all."
  [[a b x y]]
  (or
    (< b x)   ; a b x y
    (< y a))) ; x y a b



(defpure part2
  {[example] 4}
  "In how many assignment pairs do the ranges overlap?"
  [^String input]
  (count (remove separate? (parse-pairs input))))



(run-tests)
