(ns user
  (:require [clojure.string :as string]
            [clojure.test :refer [is run-tests]]))

(defmacro examples [f & coll-arguments-result]
  `(fn []
     ~@(for [[arguments result] (partition 2 coll-arguments-result)]
         `(is (= ~(cons f arguments) ~result)))))

(defn count-lanterns
  {:test (examples count-lanterns
           [[3 4 3 1 2]] [0 1 1 2 1 0 0 0 0])}
  [lanterns]            ; 0 1 2 3 4 5 6 7 8
  (let [freqs (frequencies lanterns)]
    (for [i (range 9)]
      (get freqs i 0))))

(defn tomorrow
  {:test (examples tomorrow
           [[0 1 1 2 1 0 0 0 0]] [1 1 2 1 0 0 0 0 0]
           [[1 1 2 1 0 0 0 0 0]] [1 2 1 0 0 0 1 0 1]
           [[1 2 1 0 0 0 1 0 1]] [2 1 0 0 0 1 1 1 1]
           [[2 1 0 0 0 1 1 1 1]] [1 0 0 0 1 1 3 1 2]
           [[1 0 0 0 1 1 3 1 2]] [0 0 0 1 1 3 2 2 1])}
  
  [[_0 _1 _2 _3 _4 _5    _6     _7 _8]]
  [ _1 _2 _3 _4 _5 _6 (+ _0 _7) _8 _0])

(defn both-parts
  {:test (examples both-parts
           [80] 5934
           [256] 26984457539)}
  [days]
  (as-> [3 4 3 1 2] _
    (count-lanterns _)
    (iterate tomorrow _)
    (nth _ days)
    (reduce + 0 _)))

(run-tests)
