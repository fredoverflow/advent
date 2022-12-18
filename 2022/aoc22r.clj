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
  "The scan [...] approximates the shape of the lava droplet
  with 1x1x1 cubes on a 3D grid, each given as its x,y,z position."
  
  "2,2,2
  1,2,2
  3,2,2
  2,1,2
  2,3,2
  2,2,1
  2,2,3
  2,2,4
  2,2,6
  1,2,5
  3,2,5
  2,1,5
  2,3,5")



(defpure parse-droplet
  {["1,2,3
    4,5,6"] #{[1 2 3]
              [4 5 6]}}
  [^String input]
  (into #{}
    (for [[_ & xyz] (re-seq #"(\d+),(\d+),(\d+)" input)]
      (mapv parse-long xyz))))



(defpure neighbours
  {[[15 25 26]] [[14 25 26]
                 [16 25 26]
                 
                 [15 24 26]
                 [15 26 26]
                 
                 [15 25 25]
                 [15 25 27]]}
  [[x y z]]
  
  [[(dec x) y z]
   [(inc x) y z]
   
   [x (dec y) z]
   [x (inc y) z]
   
   [x y (dec z)]
   [x y (inc z)]])



(defpure part1
  {["1,1,1
    2,1,1"]  10
   
   [example] 64}
  "Count up all the sides that aren't connected to another cube."
  [^String input]
  (let [droplet (parse-droplet input)]
    (count
      (for [cube      droplet
            neighbour (neighbours cube)
            
            :when (not (droplet neighbour))]
        neighbour))))



(defpure part2
  {[example] 58}
  "What is the exterior surface area of your scanned lava droplet?"
  [^String input]
  (let [droplet (parse-droplet input)
        
        minimum (dec (transduce cat min Long/MAX_VALUE droplet)) ; 0   -1
        maximum (inc (transduce cat max Long/MIN_VALUE droplet)) ; 7   22
        
        within-bounds? (fn [[x y z]]
                         (and
                           (<= minimum x maximum)
                           (<= minimum y maximum)
                           (<= minimum z maximum)))
        
        not-droplet (complement droplet)]
    
    (loop [border  #{[minimum minimum minimum]}
           visited border
           surface 0]
      
      (if (empty? border)
        surface
        (let [border  (for [cube      border
                            neighbour (neighbours cube)
                            
                            :when (and
                                    (within-bounds? neighbour)
                                    (not (visited neighbour)))]
                        neighbour)
              
              surface (+ surface (count (filter droplet border)))
              
              border  (into #{} (filter not-droplet) border)]
          
          (recur
            border
            (into visited border)
            surface))))))



(run-tests)
