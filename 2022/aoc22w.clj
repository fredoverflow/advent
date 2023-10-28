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



(defpure parse-grove {[".....
                       |..##.
                       |..#..
                       |.....
                       |..##.
                       |....."] #{[2 1]
                                  [3 1]
                                  [2 2]
                                  [2 4]
                                  [3 4]}}
  [^String grove]
  (let [lines (split-lines grove)]
    (set
      (for [y (range (count lines))
            :let [line (nth lines y)]
            x (range (count line))
            :when (= \# (nth line x))]
        [x y]))))


(def neighbours [[-1 -1] [ 0 -1] [+1 -1]   ; 0 1 2
                 [-1  0]         [+1  0]   ; 3   4
                 [-1 +1] [ 0 +1] [+1 +1]]) ; 5 6 7


(def triplets [[0 1 2]
               [5 6 7]
               [0 3 5]
               [2 4 7]])


(defpure plus {[[10 20]
                [ 1  2]] [11 22]}
  [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])


(defpure propositions {[triplets
                        #{[2 1]
                          [3 1]
                          [2 2]
                          [2 4]
                          [3 4]}] {[2 1] [2 0]
                                   [3 1] [3 0]
                                   [2 2] [2 3]
                                   [2 4] [2 3]
                                   [3 4] [3 3]}}
  [triplets grove]
  (into {}
    (map (fn [xy]
           (let [neighbours (mapv #(plus xy %) neighbours)]
             (if (some grove neighbours)
               {xy (or
                     (first (for [[i j k] triplets
                                  :when (not (or
                                               (grove (neighbours i))
                                               (grove (neighbours j))
                                               (grove (neighbours k))))]
                              (neighbours j)))
                     xy)}
               {xy xy}))))
    grove))


(defpure move-once {[triplets 
                     #{[2 1]
                       [3 1]
                       [2 2]
                       [2 4]
                       [3 4]}] #{[2 0]
                                 [3 0]
                                 [2 2]
                                 [3 3]
                                 [2 4]}}
  [triplets grove]
  (let [props (propositions triplets grove)
        freqs (frequencies (vals props))]
    (into #{}
      (map (fn [[stay go]]
             (if (= 1 (freqs go))
               go
               stay)))
      props)))


(defpure move-10 {[#{[2 1]
                     [3 1]
                     [2 2]
                     [2 4]
                     [3 4]}] #{[2 0]
                               [4 1]
                               [0 2]
                               [4 3]
                               [2 5]}}
  [grove]
  (loop [grove     grove
         triplets  (cycle triplets)
         countdown 10]
    (if (zero? countdown)
      grove
      (recur
        (move-once (take 4 triplets) grove)
        (rest triplets)
        (dec countdown)))))


(defpure part1 {[#{[4 0]
                   [2 1]
                   [3 1]
                   [4 1]
                   [6 1]
                   [0 2]
                   [4 2]
                   [6 2]
                   [1 3]
                   [5 3]
                   [6 3]
                   [0 4]
                   [2 4]
                   [3 4]
                   [4 4]
                   [0 5]
                   [1 5]
                   [3 5]
                   [5 5]
                   [6 5]
                   [1 6]
                   [4 6]}] 110}
  "Find the smallest rectangle that contains the Elves after 10 rounds.
  How many empty ground tiles does that rectangle contain?"
  [grove]
  (let [grove  (move-10 grove)
        min-x  (apply min (map first grove))
        max-x  (apply max (map first grove))
        min-y  (apply min (map second grove))
        max-y  (apply max (map second grove))
        width  (inc (- max-x min-x))
        height (inc (- max-y min-y))
        area   (* width height)
        elves  (count grove)]
    (- area elves)))



(defpure part2 {[#{[4 0]
                   [2 1]
                   [3 1]
                   [4 1]
                   [6 1]
                   [0 2]
                   [4 2]
                   [6 2]
                   [1 3]
                   [5 3]
                   [6 3]
                   [0 4]
                   [2 4]
                   [3 4]
                   [4 4]
                   [0 5]
                   [1 5]
                   [3 5]
                   [5 5]
                   [6 5]
                   [1 6]
                   [4 6]}] 20}
  "What is the number of the first round where no Elf moves?"
  [grove]
  (loop [grove    grove
         triplets (cycle triplets)
         round    1]
    (let [next-grove (move-once (take 4 triplets) grove)]
      (if (= grove next-grove)
        round
        (recur
          next-grove
          (rest triplets)
          (inc round))))))



(run-tests)
