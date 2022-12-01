(ns user
  (:require [clojure.string :as string]
            [clojure.test :refer [is run-tests]]))

(defmacro examples [f & coll-arguments-result]
  `(fn []
     ~@(for [[arguments result] (partition 2 coll-arguments-result)]
         `(is (= ~(cons f arguments) ~result)))))

(def demo-connections-a
  ["start" "A"
   "start" "b"
   "A" "b"
   "A" "c"
   "b" "d"
   "A" "end"
   "b" "end"])

(def demo-connections-b
  ["dc" "end"
   "HN" "start"
   "start" "kj"
   "dc" "start"
   "dc" "HN"
   "LN" "dc"
   "HN" "end"
   "kj" "sa"
   "kj" "HN"
   "kj" "dc"])

(def demo-connections-c
  ["fs" "end"
   "he" "DX"
   "fs" "he"
   "start" "DX"
   "pj" "DX"
   "end" "zg"
   "zg" "sl"
   "zg" "pj"
   "pj" "he"
   "RW" "he"
   "fs" "DX"
   "pj" "RW"
   "zg" "RW"
   "start" "pj"
   "he" "WI"
   "zg" "he"
   "pj" "fs"
   "start" "RW"])

(defn connections->system
  {:test (examples connections->system [demo-connections-a]
           {"start" #{"A" "b"}
            "A"     #{"b" "c" "end"}
            "b"     #{"A" "d" "end"}
            "d"     #{"b"}
            "c"     #{"A"}
            "end"   #{"A" "b"}})}
  [connections]
  (->> connections
    (partition 2)
    (map (fn [[a b]] (cond ; never revisit start
                       (= "start" a) {a #{b}}
                       (= "start" b)         {b #{a}}
                       :else         {a #{b}, b #{a}})))
    (apply merge-with into)))

(defn big-cave?
  "There are two types of caves:
  big caves (written in uppercase, like A) and
  small caves (written in lowercase, like b)"
  {:test (examples big-cave?
           ["A"] true
           ["b"] false
           ["ABC"] true
           ["Abc"] true
           ["abc"] false)}
  [^String name]
  (Character/isUpperCase (. name charAt 0)))

(defn part-1
  "How many paths visit small caves at most once?"
  {:test (examples part-1
           [demo-connections-a] 10
           [demo-connections-b] 19
           [demo-connections-c] 226)}
  ([connections]
    (part-1 (connections->system connections) "start"))
  
  ([system cave]
    (if (= "end" cave)
      1
      (let [neighbours (system cave)
            sub-system (if (big-cave? cave)
                         system
                         (dissoc system cave))]
        
        (transduce (map #(part-1 sub-system %)) + 0 neighbours)))))

(defn part-2
  "A single small cave can be visited at most twice"
  {:test (examples part-2
           [demo-connections-a] 36
           [demo-connections-b] 103
           [demo-connections-c] 3509)}
  ([connections]
    (part-2 (connections->system connections) "start" #{} true))
  
  ([system cave visited credit?]
    (cond
      (= "end" cave)       1
      (big-cave? cave)     (transduce (map #(part-2 system %       visited       credit?)) + 0 (system cave))
      (not (visited cave)) (transduce (map #(part-2 system % (conj visited cave) credit?)) + 0 (system cave))
      credit?              (transduce (map #(part-2 system %       visited       false  )) + 0 (system cave))
      :else                0)))

(run-tests)
