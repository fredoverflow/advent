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



(defpure parse-input
  {["LLR
    AAA = (BBB, BBB)
    BBB = (AAA, ZZZ)
    ZZZ = (ZZZ, ZZZ)"] {:directions "LLR"
                        ;             node   left right
                        :network    {"AAA" ["BBB" "BBB"]
                                     "BBB" ["AAA" "ZZZ"]
                                     "ZZZ" ["ZZZ" "ZZZ"]}}}
  "One of the documents contains a list of left/right instructions,
  and the rest of the documents seem to describe some kind of network
  of labeled nodes."
  [^String input]
  (let [[directions & triples] (re-seq #"\w+" input)]
    {:directions directions
     :network    (into {}
                   (for [[key left right] (partition 3 triples)]
                     {key [left right]}))}))

(defpure path-length
  {["LLR" {"AAA" ["BBB" "BBB"]
           "BBB" ["AAA" "ZZZ"]
           "ZZZ" ["ZZZ" "ZZZ"]} "AAA"] 6}
  "Follow the left/right instructions. How many steps are required to reach __Z?
  If you run out of left/right instructions, repeat the instructions."
  [directions, network, start-node]
  (->> (cycle directions)
    (reductions
      (fn [node direction] ((network node) (case direction
                                             \L 0
                                             \R 1)))
      start-node)
    (take-while (fn [[k e y]] (not= \Z y)))
    count))

(defpure part1
  {["RL
    AAA = (BBB, CCC)
    BBB = (DDD, EEE)
    CCC = (ZZZ, GGG)
    DDD = (DDD, DDD)
    EEE = (EEE, EEE)
    GGG = (GGG, GGG)
    ZZZ = (ZZZ, ZZZ)"] 2
   ["LLR
    AAA = (BBB, BBB)
    BBB = (AAA, ZZZ)
    ZZZ = (ZZZ, ZZZ)"] 6}
  "You feel like AAA is where you are now, and you have to
  follow the left/right instructions until you reach ZZZ."
  [^String input]
  (let [{:keys [directions network]} (parse-input input)]
    (path-length directions network "AAA")))



(run-tests)



(defn greatest-common-divisor [^long a, ^long b]
  (if (zero? b)
    a
    (recur b (rem a b))))

(defn least-common-multiple [^long a, ^long b]
  (-> a
    (quot (greatest-common-divisor a b))
    (* b)))

(defn part2
  "Start at every node that ends with A and follow all of the paths at the
  same time until they all simultaneously end up at nodes that end with Z.
  
  The loops are really well behaved. The all seem to follow something like
  A->B->C->....->Z->B->C->....
  This makes it possible to use math for finding the final solution.
  On the real input, each ghost will reach a single end node once
  (and then start the loop again)."
  [^String input]
  (let [{:keys [directions network]} (parse-input input)]
    
    (->> (for [^String node (keys network)
               :when (= \A (.charAt node 2))]
           (path-length directions network node))
      
      (reduce least-common-multiple))))
