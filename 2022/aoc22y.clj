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


(defpure split-lines {["123
                       |456
                       |789"] ["123"
                               "456"
                               "789"]}
  [^String s]
  (string/split s #"\n *+\|?"))



(defpure parse-snafu {["1=-0-2"] 1747
                      [ "12111"]  906
                      [  "2=0="]  198
                      [    "21"]   11
                      [  "2=01"]  201
                      [   "111"]   31
                      [ "20012"] 1257
                      [   "112"]   32
                      [ "1=-1="]  353
                      [  "1-12"]  107
                      [    "12"]    7
                      [    "1="]    3
                      [   "122"]   37}
  ^long [^String snafu]
  (reduce
    (fn [temp digit]
      (+ (* 5 temp) (case digit
                      \0 +0
                      \1 +1
                      \2 +2
                      \= -2
                      \- -1)))
    0
    snafu))


(defpure to-snafu {[1747] "1=-0-2"
                   [ 906]  "12111"
                   [ 198]   "2=0="
                   [  11]     "21"
                   [ 201]   "2=01"
                   [  31]    "111"
                   [1257]  "20012"
                   [  32]    "112"
                   [ 353]  "1=-1="
                   [ 107]   "1-12"
                   [   7]     "12"
                   [   3]     "1="
                   [  37]    "122"}
  ^String [^long x]
  (let [sb (new StringBuilder)]
    (loop [x x]
      (let [digit        (nth "012=-" (rem x 5))
            rounded-rest (quot (+ x 2) 5)]
        (. sb append digit)
        (if (pos? rounded-rest)
          (recur rounded-rest)
          (.. sb reverse toString))))))


(defpure part1 {["1=-0-2
                 |12111
                 |2=0=
                 |21
                 |2=01
                 |111
                 |20012
                 |112
                 |1=-1=
                 |1-12
                 |12
                 |1=
                 |122"] "2=-1=0"}
  [^String input]
  (->> input
    split-lines
    (transduce
      (map parse-snafu)
      +
      0)
    to-snafu))



(run-tests)
