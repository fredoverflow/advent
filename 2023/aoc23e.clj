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



(def example-input
  "The almanac starts by listing which seeds need to be planted.
  The rest of the almanac contains a list of maps
  which describe how to convert numbers from a source category
  into numbers in a destination category.
  Rather than list every source number and
  its corresponding destination number one by one,
  the maps describe entire ranges of numbers that can be converted."
  ;;;;;;;;;;;;;;;;;;;
  "seeds: 79 14 55 13
  
  seed-to-soil map:
  50 98 2
  52 50 48
  
  soil-to-fertilizer map:
  0 15 37
  37 52 2
  39 0 15
  
  fertilizer-to-water map:
  49 53 8
  0 11 42
  42 0 7
  57 7 4
  
  water-to-light map:
  88 18 7
  18 25 70
  
  light-to-temperature map:
  45 77 23
  81 45 19
  68 64 13
  
  temperature-to-humidity map:
  0 69 1
  1 0 69
  
  humidity-to-location map:
  60 56 37
  56 93 4")

(defpure parse-seeds
  {[example-input] [79 14 55 13]}
  [^String input]
  (->> input
    string/split-lines
    first
    (re-seq #"\d+")
    (mapv parse-long)))

(defpure preprocess-map
  {[[50 98  2]] {:begin 98, :end 100, :delta -48}
   [[52 50 48]] {:begin 50, :end  98, :delta  +2}}
  [[destination-start source-start range-length]]
  {:begin source-start
   :end   (+ source-start range-length)
   :delta (- destination-start source-start)})

(def example-maps [[{:begin 50, :end  98, :delta  +2}
                    {:begin 98, :end 100, :delta -48}]
                   
                   [{:begin  0, :end  15, :delta +39}
                    {:begin 15, :end  52, :delta -15}
                    {:begin 52, :end  54, :delta -15}]
                   
                   [{:begin  0, :end   7, :delta +42}
                    {:begin  7, :end  11, :delta +50}
                    {:begin 11, :end  53, :delta -11}
                    {:begin 53, :end  61, :delta  -4}]
                   
                   [{:begin 18, :end  25, :delta +70}
                    {:begin 25, :end  95, :delta  -7}]
                   
                   [{:begin 45, :end  64, :delta +36}
                    {:begin 64, :end  77, :delta  +4}
                    {:begin 77, :end 100, :delta -32}]
                   
                   [{:begin  0, :end  69, :delta  +1}
                    {:begin 69, :end  70, :delta -69}]
                   
                   [{:begin 56, :end  93, :delta  +4}
                    {:begin 93, :end  97, :delta -37}]])

(defpure parse-maps
  {[example-input] example-maps}
  [^String input]
  (for [conversion-map (rest (string/split input #"\w+-to-\w+ map:"))]
    (->> conversion-map
      (re-seq #"\d+")
      (into [] (comp
                 (map parse-long)
                 (partition-all 3)
                 (map preprocess-map)))
      (sort-by :begin))))

(def example-map [{:begin 98, :end 100, :delta -48}
                  {:begin 50, :end  98, :delta  +2}])

(defpure convert
  {[example-map 79] 81
   [example-map 14] 14
   [example-map 55] 57
   [example-map 13] 13}
  [conversion-map source]
  (-> (for [{:keys [begin end delta]} conversion-map
            :when (and (<= begin source) (< source end))]
        (+ source delta))
    first
    (or source)))

(defpure seed->location
  {[example-maps 79] 82
   [example-maps 14] 43
   [example-maps 55] 86
   [example-maps 13] 35}
  [maps source]
  (let [[soil fertilizer water light temperature humidity location] maps]
    (->> source
      (convert soil)
      (convert fertilizer)
      (convert water)
      (convert light)
      (convert temperature)
      (convert humidity)
      (convert location))))

(defpure part1
  {[example-input] 35}
  "What is the lowest location number that corresponds
  to any of the initial seed numbers?"
  [^String input]
  (let [seeds (parse-seeds input)
        maps  (parse-maps input)]
    (transduce
      (map (partial seed->location maps))
      min
      Long/MAX_VALUE
      seeds)))



(defpure parse-seed-pairs
  {[example-input] [{:begin 79, :end (+ 79 14)}
                    {:begin 55, :end (+ 55 13)}]}
  "The values on the initial seeds: line come in pairs.
  Within each pair, the first value is the start of the range
  and the second value is the length of the range."
  [^String input]
  (->> input
    string/split-lines
    first
    (re-seq #"\d+")
    (into [] (comp
               (map parse-long)
               (partition-all 2)
               (map (fn [[start length]]
                      {:begin  start
                       :end (+ start length)}))))))

(defn singleton-range [begin end]
  (if (< begin end)
    (java.util.Collections/singletonList {:begin begin, :end end})))

(defn split [{a :begin, z :end} conversion-map]
  (into []
    cat
    (let [moved (for [{:keys [begin end delta]} conversion-map
                      :let [b (max a begin)
                            e (min end z)]
                      :when (< b e)]
                  {:b b
                   :e e
                   :begin (+ b delta)
                   :end   (+ e delta)})
          
          between (for [[{:keys [e]} {:keys [b]}] (partition 2 1 moved)
                        :when (< b e)]
                    {:begin e
                     :end   b})]
      
      [(singleton-range a, (-> conversion-map first :begin (min z)))
       moved, between
       (singleton-range (-> conversion-map last :end (max a)), z)])))

(defpure part2
  {[example-input] 46}
  "Consider all of the initial seed numbers
  listed in the ranges on the first line of the almanac.
  What is the lowest location number
  that corresponds to any of the initial seed numbers?"
  [^String input]
  (let [seed-pairs (parse-seed-pairs input)
        maps       (parse-maps       input)
        
        [soil fertilizer water light temperature humidity location] maps]
    
    (transduce (map :begin) min Long/MAX_VALUE
      (for [range seed-pairs
            range (split range soil)
            range (split range fertilizer)
            range (split range water)
            range (split range light)
            range (split range temperature)
            range (split range humidity)
            range (split range location)]
        range))))



(run-tests)
