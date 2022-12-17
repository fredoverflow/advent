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
  "In jet patterns, < means a push to the left, while > means a push to the right.
  If the end of the list is reached, it repeats."
  
  ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")



(def rocks [[33 34 35 36]
            
            [   14
             23 24 25
             ,  34]
            
            [      15
             ,     25
             33 34 35]
            
            [03
             13
             23
             33]
            
            [23 24
             33 34]])



(defn empty-chamber ^chars []
  (char-array
    (for [line (concat
                 (repeat 8000 "|       |\n")
                 (repeat    1 "+-------+\n"))
          ch   line]
      ch)))



(defn line-empty? [^chars chamber, ^long location]
  (and
    (= \space (aget chamber (+ location 1)))
    (= \space (aget chamber (+ location 2)))
    (= \space (aget chamber (+ location 3)))
    (= \space (aget chamber (+ location 4)))
    (= \space (aget chamber (+ location 5)))
    (= \space (aget chamber (+ location 6)))
    (= \space (aget chamber (+ location 7)))))



(defn spawn-location [^chars chamber]
  (loop [location (- 80000 70)]
    (if (and
          (line-empty? chamber (+ location 40))
          (line-empty? chamber (+ location 50))
          (line-empty? chamber (+ location 60)))
      location
      (recur (- location 10)))))



(defn collides? [^chars chamber, ^long location, rock]
  (some
    (fn [delta]
      (not= \space
        (aget chamber (+ location delta))))
    rock))



(defn place-rock [^chars chamber, ^long location, rock]
  (doseq [delta rock]
    (aset chamber (+ location delta) \@)))



(defpure part1
  {[example] 3068}
  "How many units tall will the tower of rocks be
  after 2022 rocks have stopped falling?"
  [^String input]
  (let [chamber (empty-chamber)]
    (loop [location       (spawn-location chamber)
           rocks          (take 2022 (cycle rocks))
           [wind & winds] (cycle (mapv {\< -1
                                        \> +1} input))]
      (if rocks
        (let [rock     (first rocks)
              
              location (if (collides? chamber (+ location wind) rock)
                         location
                         (+ location wind))]
          
          (if (collides? chamber (+ location 10) rock)
            (do
              (place-rock chamber location rock)
              (recur (spawn-location chamber) (next rocks) winds))
            (recur (+ location 10) rocks winds)))
        
        (- 7993 (quot (spawn-location chamber) 10))))))



(run-tests)
