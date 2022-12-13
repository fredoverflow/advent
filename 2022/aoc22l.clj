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

(defn debug-terrain [terrain width]
  (->> (new String terrain)
    (partition width)
    (map string/join)
    (string/join "\n")))



(def example
  "~~~~~~~~
  Sabqponm
  abcryxxl
  accszExk
  acctuvwj
  abdefghi
  ~~~~~~~~~")



(defpure part1
  {[example] 31}
  [^String input]
  (let [terrain    (string/replace input #"\n *" "~") ; ..._Sabqponm_abcryxxl_accszExk_...
        width      (count (re-find #"~+" terrain))    ; 9
        directions [-1 +1 (- width) (+ width)]        ; [-1 +1 -9 +9]
        
        start      (string/index-of terrain \S)       ;  9
        end        (string/index-of terrain \E)       ; 32
        
        terrain    (string/replace terrain \S \a)     ; S -> a
        terrain    (string/replace terrain \E \z)     ; E -> z
        terrain    (. terrain getBytes)               ; \a...\z -> 97...122
        
        VISITED    (byte \~)] ; 126
    
    (loop [hikers [[start (byte \a) 0]]]
      
      (let [hikers (for [[position height distance] hikers
                         
                         :when (if (not= (aget terrain position) VISITED)
                                 (aset terrain position VISITED))
                         
                         :let [max-height (inc height)
                               distance   (inc distance)]
                         
                         direction directions
                         
                         :let [neighbour (+ position direction)
                               height    (aget terrain neighbour)]
                         
                         :when (<= height max-height)]
                     
                     [neighbour height distance])
            
            [winner] (filterv
                       (fn [[position height distance]]
                         (= end position))
                       hikers)]
        
        (println (debug-terrain terrain width) hikers winner)
        
        (if winner
          ((fn [[position height distance]] distance) winner)
          (recur hikers))))))



(defpure part2
  {[example] 29}
  [^String input]
  (let [terrain    (string/replace input #"\n *" "~")
        width      (count (re-find #"~+" terrain))
        directions [-1 +1 (- width) width]
        
        start      (string/index-of terrain \S)
        end        (string/index-of terrain \E)
        
        terrain    (string/replace terrain \S \a)
        terrain    (string/replace terrain \E \z)
        terrain    (. terrain getBytes)
        
        VISITED    (byte \~)]
    ;               ___ _________
    (loop [hikers [[end (byte \z) 0]]]
      
      (let [hikers (for [[position height distance] hikers
                         
                         :when (if (not= (aget terrain position) VISITED)
                                 (aset terrain position VISITED))
                         ;     __________ ____________
                         :let [min-height (dec height)
                               distance   (inc distance)]
                         
                         direction directions
                         
                         :let [neighbour (+ position direction)
                               height    (aget terrain neighbour)]
                         ;      __        __________
                         :when (>= height min-height)]
                     
                     [neighbour height distance])
            
            [winner] (filterv
                       (fn [[position height distance]]
                         ;  ______ _________
                         (= height (byte \a)))
                       hikers)]
        
        (println (debug-terrain terrain width) hikers winner)
        
        (if winner
          ((fn [[position height distance]] distance) winner)
          (recur hikers))))))



(run-tests)
