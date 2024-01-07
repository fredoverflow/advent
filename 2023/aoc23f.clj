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



(defn solve-quadratic-equation
  "Solves x² + px + q = 0 for x"
  [p q]
  (let [foo (* -0.5 p)
        bar (Math/sqrt (- (* foo foo) q))]
    [(- foo bar)
     (+ foo bar)]))

(defpure part2
  {[{:time 71530, :record-distance 940200}] 71503}
  "Your toy boat has a starting speed of zero millimeters per millisecond.
  For each whole millisecond you spend at the beginning of the race holding down the button,
  the boat's speed increases by one millimeter per millisecond."
  [{:keys [time record-distance]}]
  (let [[x1
         x2] (solve-quadratic-equation (- time) record-distance)
        
        x1 (long (Math/floor x1))
        x2 (long (Math/ceil  x2))]
    
    (dec (- x2 x1))))



(defpure part1
  {[[{:time  7, :record-distance   9}
     {:time 15, :record-distance  40}
     {:time 30, :record-distance 200}]] (* 4 8 9)}
  "Determine the number of ways you could beat the record in each race.
  What do you get if you multiply these numbers together?"
  [races]
  (transduce (map part2) * 1 races))



(run-tests)
