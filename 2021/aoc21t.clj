(ns user
  (:require [clojure.string :as string]
            [clojure.test :refer [is run-tests]]))

(defmacro examples [f & coll-arguments-result]
  `(fn []
     ~@(for [[arguments result] (partition 2 coll-arguments-result)]
         `(is (= ~(cons f arguments) ~result)))))

(def demo
  {:enhancement (-> "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
                  #..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###
                  .######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.
                  .#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....
                  .#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..
                  ...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....
                  ..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"
                  (string/replace #"\s" ""))
   
   :image ["......."
           ".#..#.."
           ".#....."
           ".##..#."
           "...#..."
           "...###."
           "......."]})

(defn safe-get [coll, ^long index]
  (->> index
    (max 0)
    (min (dec (count coll)))
    (nth coll)))

(defn lit? [^Character ch]
  (= ch \#))

(defn bit ^long [^Character ch, ^long shift]
  (if (lit? ch)
    (bit-shift-left 1 shift)
    0))

(defn enhance-once [enhancement image]
  (let [height (count image)
        width (count (image 0))]
    (vec
      (for [y (range -1 (inc height))]
        (apply str
          (for [x (range -1 (inc width))]
            (let [a (-> image (safe-get (dec y)) (safe-get (dec x)) (bit 8))
                  b (-> image (safe-get (dec y)) (safe-get      x ) (bit 7))
                  c (-> image (safe-get (dec y)) (safe-get (inc x)) (bit 6))
                  
                  d (-> image (safe-get      y ) (safe-get (dec x)) (bit 5))
                  e (-> image (safe-get      y ) (safe-get      x ) (bit 4))
                  f (-> image (safe-get      y ) (safe-get (inc x)) (bit 3))
                  
                  g (-> image (safe-get (inc y)) (safe-get (dec x)) (bit 2))
                  h (-> image (safe-get (inc y)) (safe-get      x ) (bit 1))
                  i (-> image (safe-get (inc y)) (safe-get (inc x)) (bit 0))]
              
              (nth enhancement (bit-or a b c d e f g h i)))))))))

(defn both-parts
  "Apply the image enhancement algorithm n times.
  How many pixels are lit in the resulting image?"
  [^String enhancement, image, ^long n]
  (->> image
    (iterate (partial enhance-once enhancement))
    (drop n)
    first
    (apply str)
    (filter lit?)
    count))

(defn part-1
  "Apply the image enhancement algorithm twice.
  How many pixels are lit in the resulting image?"
  {:test (examples part-1 [demo] 35)}
  [{:keys [enhancement image]}]
  (both-parts enhancement image 2))

(defn part-2
  "Apply the image enhancement algorithm 50 times.
  How many pixels are lit in the resulting image?"
  {:test (examples part-2 [demo] 3351)}
  [{:keys [enhancement image]}]
  (both-parts enhancement image 50))

(run-tests)
