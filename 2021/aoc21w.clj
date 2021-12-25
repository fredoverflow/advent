(ns user
  (:require [clojure.string :as string]
            [clojure.data.priority-map :refer [priority-map]]
            [clojure.test :refer [deftest is run-tests]]))

(defmacro examples [f & coll-arguments-result]
  `(fn []
     ~@(for [[arguments result] (partition 2 coll-arguments-result)]
         `(is (= ~(cons f arguments) ~result)))))

(def demo-burrow-1
  [nil nil '(:B :A) nil '(:C :D) nil '(:B :C) nil '(:D :A) nil nil, 0])

(def demo-burrow-2
  [nil nil '(:B :D :D :A) nil '(:C :C :B :D) nil '(:B :B :A :C) nil '(:D :A :C :A) nil nil, 0])

(def ^:const ^long cost
  "The cost is stored at the end of the burrow vector"
  11)

(def pod-weight
  {:A 1, :B 10, :C 100, :D 1000})

(def ^:dynamic stack-depth
  "2 in part 1, and 4 in part 2"
  2)

; #############
; #  A B C D  #
; ### # # # ###
;   # # # # #
;   #########

(def hallway
  [0 1 3 5 7 9 10])

(def side-rooms
  {:A 2, :B 4, :C 6, :D 8})

(defn inbetweens-exclusive [v, ^long a, ^long b]
  (if (< a b)
    (subvec v (inc a) b)
    (subvec v (inc b) a)))

(defn inbetweens-inclusive [v, ^long a, ^long b]
  (if (< a b)
    (subvec v a (inc b))
    (subvec v b (inc a))))

(defn valid-moves [burrow]
  (concat
    (for [[pod room] side-rooms
          :let [stack (burrow room)]
          :when (not-every? #{pod} stack)
          :let [[top & pop] stack]
          space hallway
          :when (not-any? keyword? (inbetweens-inclusive burrow room space))]
      (-> burrow
        (assoc  room  pop)
        (assoc  space top)
        (update cost  + (* (pod-weight top)
                          (+ (Math/abs (- room space))
                            (- stack-depth (count pop)))))))
    
    (for [space hallway
          :let [pod   (burrow space)]
          :when (keyword? pod)
          :let [room  (side-rooms pod)
                stack (burrow room)]
          :when (not-any? keyword? (inbetweens-exclusive burrow space room))
          :when (every? #{pod} stack)]
      (-> burrow
        (assoc  space nil)
        (update room  conj pod)
        (update cost  +    (* (pod-weight pod)
                             (+ (Math/abs (- space room))
                               (- stack-depth (count stack)))))))))

(defn solution? [burrow]
  (every?
    (fn [[pod room]]
      (= (repeat stack-depth pod) (burrow room)))
    side-rooms))

(defn cheapest [burrow]
  (loop [visited #{}
         border  (priority-map burrow 0)]
    (let [[cheapest-move cheapest-cost] (peek border)]
      (if (solution? cheapest-move)
        cheapest-cost
        (recur
          (conj visited cheapest-move)
          (into (pop border)
            (for [move (valid-moves cheapest-move)
                  :let [cost (move 11)]
                  :when (not (visited move))]
              [move cost])))))))

(defn part-1
  {:test (examples part-1 [demo-burrow-1] 12521)}
  [burrow]
  (binding [stack-depth 2]
    (cheapest burrow)))

(defn part-2
  {:test (examples part-2 [demo-burrow-2] 44169)}
  [burrow]
  (binding [stack-depth 4]
    (cheapest burrow)))

(run-tests)
