(ns user
  (:require [clojure.test :refer [is run-tests]]))

(defmacro examples [f & coll-arguments-result]
  `(fn []
     ~@(for [[arguments result] (partition 2 coll-arguments-result)]
         `(is (= ~(cons f arguments) ~result)))))

(def demo-commands [:forward 5, :down 5, :forward 8, :up 3, :down 8, :forward 2])

(defn part-1
  {:test (examples part-1 [demo-commands] 150)}
  [commands]
  (let [start {:horizontal 0, :depth 0}
        end (reduce
              (fn [state [direction amount]]
                (case direction
                  :forward (update state :horizontal #(+ % amount))
                  :down    (update state :depth      #(+ % amount))
                  :up      (update state :depth      #(- % amount))))
              start
              (partition 2 commands))]
    (* (end :horizontal) (end :depth))))

(defn part-2
  {:test (examples part-2 [demo-commands] 900)}
  [commands]
  (let [start {:horizontal 0, :depth 0, :aim 0}
        end (reduce
              (fn [state [direction amount]]
                (case direction
                  :forward (-> state
                             (update :horizontal #(+ % amount))
                             (update :depth      #(+ % (* amount (state :aim)))))
                  :down    (update state :aim    #(+ % amount))
                  :up      (update state :aim    #(- % amount))))
              start
              (partition 2 commands))]
    (* (end :horizontal) (end :depth))))

(run-tests)
