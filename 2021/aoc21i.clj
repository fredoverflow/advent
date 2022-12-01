(ns user
  (:require [clojure.string :as string]
            [clojure.test :refer [is run-tests]]))

(defmacro examples [f & coll-arguments-result]
  `(fn []
     ~@(for [[arguments result] (partition 2 coll-arguments-result)]
         `(is (= ~(cons f arguments) ~result)))))

(def demo-heightmap
  ["::::::::::::"
   ":2199943210:"
   ":3987894921:"
   ":9856789892:"
   ":8767896789:"
   ":9899965678:"
   "::::::::::::"])

(defn low-points [heightmap]
  (let [height (count  heightmap   )
        width  (count (heightmap 1))]
    
    (for [y (range 1 (dec height))
          x (range 1 (dec width ))
          
          :let [here  (long (nth (heightmap      y )      x ))
                east  (long (nth (heightmap      y ) (inc x)))
                north (long (nth (heightmap (dec y))      x ))
                west  (long (nth (heightmap      y ) (dec x)))
                south (long (nth (heightmap (inc y))      x ))]
          
          :when (and
                  (< here east)
                  (< here north)
                  (< here west)
                  (< here south))]
      
      (- here 48))))

(defn part-1
  {:test (examples part-1 [demo-heightmap] 15)}
  [heightmap]
  (reduce
    (fn [result lowpoint]
      (+ result (inc lowpoint)))
    0
    (low-points heightmap)))

(defn flood-fill ^long [grid, ^long y, ^long x]
  (let [row (aget grid y)
        z   (aget row  x)]
    
    (if (>= z 9)
      0
      (do
        (aset-byte row x 9)
        (+ 1
          (flood-fill grid      y  (inc x))
          (flood-fill grid (dec y)      x )
          (flood-fill grid      y  (dec x))
          (flood-fill grid (inc y)      x ))))))

(defn basins [heightmap]
  (let [height (count  heightmap   )
        width  (count (heightmap 1))
        
        grid   (->> #(byte-array width)
                 repeatedly
                 (take height)
                 object-array)]
    
    (doseq [y (range 0 height)
            x (range 0 width )]
      
      (aset-byte
        (aget grid y)
        x
        (- (byte (nth (heightmap y) x)) 48)))
    
    (for [y (range 1 (dec height))
          x (range 1 (dec width ))
          
          :let [basin (flood-fill grid y x)]
          :when (pos? basin)]
      basin)))

(defn part-2
  {:test (examples part-2 [demo-heightmap] 1134)}
  [heightmap]
  (->> heightmap
    basins
    (sort (java.util.Collections/reverseOrder))
    (take 3)
    (apply *)))

(run-tests)
