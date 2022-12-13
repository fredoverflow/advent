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



(def example [[1,1,3,1,1]    ; aacaa
              [1,1,5,1,1]    ; aaeaa
              
              [[1], [2,3,4]] ; bcd
              [[1],  4     ] ; d
              
              [ 9     ]      ; i
              [[8,7,6]]      ; hgf
              
              [[4,4], 4,4]   ; dd
              [[4,4], 4,4,4] ; ddd
              
              [7,7,7,7]      ; gggg
              [7,7,7]        ; ggg
              
              [ ]            ; 
              [3]            ; c
              
              [[[]]]
              [[  ]]
              
              [1,[2,[3,[4,[5,6, 7]]]],8,9]
              [1,[2,[3,[4,[5,6, 0]]]],8,9]])



(defn compare-values [left right]
  (cond
    (and
      (number? left)
      (number? right)) (compare  left  right)
    
    (number? left)     (recur   [left] right)
    
    (number? right)    (recur    left [right])
    
    :assume-vectors    (or
                         (->> (map compare-values left right)
                           (drop-while zero?)
                           first)
                         (- (count left) (count right)))))



(defpure part1
  {[example] 13}
  "Determine which pairs of packets are already in the right order.
  What is the sum of the indices of those pairs?"
  [^String input]
  (transduce
    (comp
      (partition-all 2)
      (keep-indexed (fn [index [left right]]
                      (if (neg? (compare-values left right))
                        (inc index))))) ; The first pair has index 1
    +
    0
    input))



(defpure part2
  {[example] 140}
  "Organize all of the packets (incl. two divider packets) into the correct order.
  Determine the indices of the two divider packets and multiply them together."
  [^String input]
  (let [divider-packets #{[[2]] [[6]]}]
    (transduce
      (keep-indexed (fn [index packet]
                      (if (divider-packets packet)
                        (inc index)))) ; The first packet is at index 1
      *
      1
      (sort compare-values (into input divider-packets)))))



(run-tests)
