(ns user
  (:require [clojure.test :refer [is run-tests]]))

(defmacro examples [f & coll-arguments-result]
  `(fn []
     ~@(for [[arguments result] (partition 2 coll-arguments-result)]
         `(is (= ~(cons f arguments) ~result)))))

(def demo-numbers [199 200 208 210 200 207 240 269 260 263])

(defn part-1
  {:test (examples part-1 [demo-numbers] 7)}
  [numbers]
  (count
    (for [[a b] (partition 2 1 numbers)
          :when (< a b)]
      nil)))

(defn part-2
  {:test (examples part-2 [demo-numbers] 5)}
  [numbers]
  (count
    (for [[a _ _ b] (partition 4 1 numbers)
          :when (< a b)]
      nil)))

(run-tests)
