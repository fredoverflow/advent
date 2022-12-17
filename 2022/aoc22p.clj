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
  "You scan the cave and discover a network
  of pipes and pressure-release valves."
  
  "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
  Valve BB has flow rate=13; tunnels lead to valves CC, AA
  Valve CC has flow rate=2; tunnels lead to valves DD, BB
  Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
  Valve EE has flow rate=3; tunnels lead to valves FF, DD
  Valve FF has flow rate=0; tunnels lead to valves EE, GG
  Valve GG has flow rate=0; tunnels lead to valves FF, HH
  Valve HH has flow rate=22; tunnel leads to valve GG
  Valve II has flow rate=0; tunnels lead to valves AA, JJ
  Valve JJ has flow rate=21; tunnel leads to valve II")



(defpure parse-cave
  {[example] {:valve "AA"
              :score 0
              "AA" {:flow  0, :tunnels ["DD" "II" "BB"]}
              "BB" {:flow 13, :tunnels ["CC" "AA"]}
              "CC" {:flow  2, :tunnels ["DD" "BB"]}
              "DD" {:flow 20, :tunnels ["CC" "AA" "EE"]}
              "EE" {:flow  3, :tunnels ["FF" "DD"]}
              "FF" {:flow  0, :tunnels ["EE" "GG"]}
              "GG" {:flow  0, :tunnels ["FF" "HH"]}
              "HH" {:flow 22, :tunnels ["GG"]}
              "II" {:flow  0, :tunnels ["AA" "JJ"]}
              "JJ" {:flow 21, :tunnels ["II"]}}}
  [^String input]
  (into {:valve "AA"
         :score 0}
    (for [[       _      valve               flow                             tunnels]
          (re-seq #"Valve (..) has flow rate=(\d+); tunnels? leads? to valves? (.+)" input)]
      [valve {:flow    (parse-long flow)
              :tunnels (string/split tunnels #", ")}])))



(defn next-states [state, ^long countdown]
  (let [{:keys [valve score]} state
        
        {:keys [flow tunnels]} (state valve)]
    
    (into (if (pos? flow)
            [(-> state
               (assoc-in [valve :flow] 0)
               (assoc :score (+ score (* countdown flow))))]
            [])
      
      (map (fn [tunnel]
             (assoc state :valve tunnel)))
      
      tunnels)))



(defpure part1 ; 42 seconds
  {[example] 1651}
  "Work out the steps to release the most pressure in 30 minutes.
  What is the most pressure you can release?"
  [^String input]
  (loop [countdown 29
         states    #{(parse-cave input)}
         border    states]
    
    (if (zero? countdown)
      (transduce (map :score) max 0 states)
      
      (let [border (into #{}
                     (comp
                       (map #(next-states % countdown))
                       cat
                       (remove states))
                     border)]
        (recur
          (dec countdown)
          (into states border)
          border)))))



(run-tests)
