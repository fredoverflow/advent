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
  "Blueprint 1:
  Each ore robot costs 4 ore.
  Each clay robot costs 2 ore.
  Each obsidian robot costs 3 ore and 14 clay.
  Each geode robot costs 2 ore and 7 obsidian.
  
  Blueprint 2:
  Each ore robot costs 2 ore.
  Each clay robot costs 3 ore.
  Each obsidian robot costs 3 ore and 8 clay.
  Each geode robot costs 3 ore and 12 obsidian.")



(defpure parse-blueprints
  {[example] [[[ 4  0  0  0]
               [ 2  0  0  0]
               [ 3 14  0  0]
               [ 2  0  7  0]]
              
              [[ 2  0  0  0]
               [ 3  0  0  0]
               [ 3  8  0  0]
               [ 3  0 12  0]]]}
  [^String input]
  (for [[_ a b c d e f] (partition 7 (mapv parse-long (re-seq #"\d+" input)))]
    [[ a  0  0  0]
     [ b  0  0  0]
     [ c  d  0  0]
     [ e  0  f  0]]))



(defpure max-robots
  {[[[ 4  0  0  0]
     [ 2  0  0  0]
     [ 3 14  0  0]
     [ 2  0  7  0]]] [4 14 7 Long/MAX_VALUE]}
  "If 4 ores is the maximum ore we can spend per minute,
  there is no point in producing more than 4 ore robots,
  as they would accumulate more ore than we could ever spend."
  [matrix]
  (assoc (apply mapv max matrix) 3 Long/MAX_VALUE))



(defn next-states [blueprint max-robots [minerals robots]]
  (let [minerals' [(+ (minerals 0) (robots 0))
                   (+ (minerals 1) (robots 1))
                   (+ (minerals 2) (robots 2))
                   (+ (minerals 3) (robots 3))]]
    
    (into #{[minerals' robots]}
      (for [i [0 1 2 3]
            :when (< (robots i) (max-robots i))
            
            :let [cost (blueprint i)]
            :when (and
                    (>= (minerals 0) (cost 0))
                    (>= (minerals 1) (cost 1))
                    (>= (minerals 2) (cost 2))
                    (>= (minerals 3) (cost 3)))]
        
        [[(- (minerals' 0) (cost 0))
          (- (minerals' 1) (cost 1))
          (- (minerals' 2) (cost 2))
          (- (minerals' 3) (cost 3))]
         
         (update robots i inc)]))))



(defn maximize-geodes [blueprint]
  (let [max-robots (max-robots blueprint)]
    (loop [minutes 24
           border  #{[[0 0 0 0]
                      [1 0 0 0]]}
           visited border]
      (if (zero? minutes)
        (transduce
          (map (fn [[minerals]]
                 (minerals 3)))
          max
          Long/MIN_VALUE
          visited)
        (let [border (into #{}
                       (comp
                         (map #(next-states blueprint max-robots %))
                         cat
                         (remove visited))
                       border)]
          (recur
            (dec minutes)
            border
            (into visited border)))))))



(defpure part1
  {[example] 33}
  "Determine the quality level of each blueprint by multiplying
  that blueprint's ID number with the largest number of geodes
  that can be opened in 24 minutes using that blueprint.
  
  Add up the quality level of all of the blueprints in your list."
  [^String input]
  (transduce
    (map-indexed (fn [index blueprint]
                   (* (inc index) (maximize-geodes blueprint))))
    +
    0
    (parse-blueprints input)))



(time (run-tests))
