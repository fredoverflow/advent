(ns user
  (:require [clojure.string :as string]
            [clojure.test :refer [is run-tests]]))

(defmacro examples [f & coll-arguments-result]
  `(fn []
     ~@(for [[arguments result] (partition 2 coll-arguments-result)]
         `(is (= ~(cons f arguments) ~result)))))

(def demo-grid
  [5 4 8 3 1 4 3 2 2 3
   2 7 4 5 8 5 4 7 1 1
   5 2 6 4 5 5 6 1 7 3
   6 1 4 1 3 3 6 1 4 6
   6 3 5 7 3 8 5 4 7 8
   4 1 6 7 5 2 4 6 4 5
   2 1 7 6 8 4 1 7 2 1
   6 8 8 2 8 8 1 1 3 4
   4 8 4 6 8 4 8 5 5 4
   5 2 8 3 7 5 1 5 2 6])

(defn pad
  "Converts 10x10 vector into padded, mutable 12x12 int-array"
  [ten-by-ten]
  (let [grid (int-array 144 Integer/MIN_VALUE)]
    (dotimes [y 10]
      (dotimes [x 10]
        (aset-int grid (+ 13 (* y 12) x)
          (ten-by-ten  (+    (* y 10) x)))))
    grid))

(defn increase
  "Increases grid element at position if element >= required-minimum
  Useful values are 0 for normal increase and 1 for flash increase"
  [grid, ^long position, ^long required-minimum]
  (let [level (aget grid position)]
    (if (>= level required-minimum)
      (aset-int grid position (inc level)))))

(defn flash
  "Each octopus [...] flashes brightly for a moment when its energy is full"
  [grid]
  (loop [flashes  (volatile! 0)
         flashed? (volatile! false)]
    ; Then, any octopus with an energy level greater than 9 flashes.
    (dotimes [pos 144]
      (when (> (aget grid pos) 9)
        (vswap!  flashes inc)
        (vreset! flashed? true)
        ; This increases the energy level of all adjacent octopuses by 1,
        ; including octopuses that are diagonally adjacent.
        (doseq [dir [-13 -12 -11
                     -1       +1
                     +11 +12 +13]]
          ; An octopus can only flash at most once per step.
          (increase grid (+ pos dir) 1))
        ; Finally, any octopus that flashed during this step has its energy level set to 0,
        ; as it used all of its energy to flash.
        (aset-int grid pos 0)))
    (if @flashed?
      ; This process continues as long as new octopuses keep having their energy level increased beyond 9.
      (recur flashes (volatile! false))
      @flashes)))

(defn part-1
  "How many total flashes are there after 100 steps?"
  {:test (examples part-1 [demo-grid] 1656)}
  [grid]
  (let [grid  (pad grid)
        total (volatile! 0)]
    (dotimes [_ 100]
      ; First, the energy level of each octopus increases by 1.
      (dotimes [pos 144]
        (increase grid pos 0))
      ; Then, any octopus with an energy level greater than 9 flashes.
      (let [flashes (flash grid)]
        (vswap! total + flashes)))
    @total))

(defn part-2
  "What is the first step during which all octopuses flash?"
  {:test (examples part-2 [demo-grid] 195)}
  [grid]
  (let [grid (pad grid)]
    (loop [step 1]
      ; First, the energy level of each octopus increases by 1.
      (dotimes [pos 144]
        (increase grid pos 0))
      ; Then, any octopus with an energy level greater than 9 flashes.
      (let [flashes (flash grid)]
        (if (= 100 flashes)
          step
          (recur (inc step)))))))

(run-tests)
