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
  [{:items       [79, 98]
    :operation   #(* % 19)
    :denominator 23
    true         2
    false        3}
   
   {:items       [54, 65, 75, 74]
    :operation   #(+ % 6)
    :denominator 19
    true         2
    false        0}
   
   {:items       [79, 60, 97]
    :operation   #(* % %)
    :denominator 13
    true         1
    false        3}
   
   {:items       [74]
    :operation   #(+ % 3)
    :denominator 17
    true         0
    false        1}])



(defn mutable-monkey [monkey]
  (-> monkey
    (update :items   #(new java.util.ArrayList %))
    (assoc  :counter (volatile! 0))))



(defn divisible? [numerator denominator]
  (zero? (rem numerator denominator)))



(defpure part1
  {[example] 10605}
  [monkeys]
  (let [monkeys (mapv mutable-monkey monkeys)]
    (dotimes [_ 20]
      (doseq [{:keys [items counter operation denominator] :as monkey} monkeys]
        (vswap! counter + (count items))
        
        (doseq [item items]
          (let [item      (quot (operation item) 3)
                recipient (monkey (divisible? item denominator))
                sink      (:items (monkeys recipient))]
            (. sink add item)))
        
        (. items clear)))
    
    (->> monkeys
      (mapv (comp deref :counter))
      (sort >)
      (transduce (take 2) * 1))))



(defpure part2
  {[example] 2713310158}
  [monkeys]
  (let [monkeys      (mapv mutable-monkey monkeys)
        shared-denom (transduce (map :denominator) * 1 monkeys)]
    (dotimes [_ 10000]
      (doseq [{:keys [items counter operation denominator] :as monkey} monkeys]
        (vswap! counter + (count items))
        
        (doseq [item items]
          (let [item      (rem (operation item) shared-denom)
                recipient (monkey (divisible? item denominator))
                sink      (:items (monkeys recipient))]
            (. sink add item)))
        
        (. items clear)))
    
    (->> monkeys
      (mapv (comp deref :counter))
      (sort >)
      (transduce (take 2) * 1))))



(run-tests)
