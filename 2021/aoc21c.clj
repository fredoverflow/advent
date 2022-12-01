(ns user
  (:require [clojure.string :as string]
            [clojure.test :refer [is run-tests]]))

(defmacro examples [f & coll-arguments-result]
  `(fn []
     ~@(for [[arguments result] (partition 2 coll-arguments-result)]
         `(is (= ~(cons f arguments) ~result)))))

(def demo-report
  ["00100"
   "11110"
   "10110"
   "10111"
   "10101"
   "01111"
   "00111"
   "11100"
   "10000"
   "11001"
   "00010"
   "01010"])

(defn transpose
  "Flips a matrix over its diagonal"
  {:test (examples transpose [[[1 2 3]
                               [4 5 6]]] [[1 4]
                                          [2 5]
                                          [3 6]])}
  [coll2d]
  ; https://stackoverflow.com/questions/10347315
  (apply mapv vector coll2d))

(defn gamma
  {:test (examples gamma [demo-report] 22)}
  [report]
  (Integer/parseInt
    (apply str
      (for [column (transpose report)]
        (max-key (frequencies column) \0 \1)))
    2))

(defn epsilon
  {:test (examples epsilon [demo-report] 9)}
  [report]
  (Integer/parseInt
    (apply str
      (for [column (transpose report)]
        (min-key (frequencies column) \1 \0)))
    2))

(defn part-1
  {:test (examples part-1 [demo-report] 198)}
  [report]
  (* (gamma report) (epsilon report)))


(defn oxygen
  {:test (examples oxygen [demo-report] 23)}
  [report]
  (loop [column 0
         report report]
    (let [groups (group-by #(nth % column) report)
          winners (max-key count (groups \0) (groups \1))]
      (if (= 1 (count winners))
        (Integer/parseInt (winners 0) 2)
        (recur (inc column) winners)))))

(defn co2
  {:test (examples co2 [demo-report] 10)}
  [report]
  (loop [column 0
         report report]
    (let [groups (group-by #(nth % column) report)
          winners (min-key count (groups \1) (groups \0))]
      (if (= 1 (count winners))
        (Integer/parseInt (winners 0) 2)
        (recur (inc column) winners)))))

(defn part-2
  {:test (examples part-2 [demo-report] 230)}
  [report]
  (* (oxygen report) (co2 report)))

(run-tests)
