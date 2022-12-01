(ns user
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is run-tests]]))

(defmacro examples [f & coll-arguments-result]
  `(fn []
     ~@(for [[arguments result] (partition 2 coll-arguments-result)]
         `(is (= ~(cons f arguments) ~result)))))

(def z-config [ 1,  1,  1,  26,  1,  1,  1,  26, 26,  1,  26, 26,  26, 26])
(def x-config [11, 10, 13, -10, 11, 11, 12, -11, -7, 13, -13,  0, -11,  0])
(def w-config [ 1, 10,  2,   5,  6,  0, 16,  12, 15,  7,   6,  5,   6, 15])

(defn valid-model-numbers
  "https://www.reddit.com/r/adventofcode/comments/rnqabd
  
  I think you've gotten to the point that you understand
  that this program runs the same(-ish) code 14 times,
  and that each code seems to take some existing data from z,
  add stuff at various points, multiply by 26, divide by 26,
  take the remainder when modified by 26, etc.
  
  See if you can understand how z is being used as a data structure for the code."
  
  ([] (flatten (valid-model-numbers 0 0 0)))
  
  ([^long model-number, ^long i, ^long z]
    (if (= 14 i)
      model-number
      ; Submarine model numbers are always fourteen-digit numbers
      ; consisting only of digits 1 through 9.
      ; The digit 0 cannot appear in a model number.
      (for [w [9 8 7 6 5 4 3 2 1]
            
            :let [pop?  (= 26 (z-config i))
                  x     (+ (rem z 26) (x-config i))
                  push? (not= x w)]
            
            :when (not= pop? push?)
            
            :let [z (cond
                      pop?  (quot z 26)
                      push? (+ (* z 26) w (w-config i)))]]
        
        (valid-model-numbers (-> model-number (* 10) (+ w)) (inc i) z)))))

(defn part-1
  "What is the largest model number accepted by MONAD?"
  {:test (examples part-1 [] 89913949293989)}
  []
  (first (valid-model-numbers)))

(defn part-2
  "What is the smallest model number accepted by MONAD?"
  {:test (examples part-2 [] 12911816171712)}
  []
  (time (last (valid-model-numbers))))

(run-tests)
