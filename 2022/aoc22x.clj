(ns user
  (:require [clojure.string :as string]
            [clojure.test :refer [do-report run-tests]]))


(defn- filter-stack-trace! [^Throwable throwable]
  (->> (for [^StackTraceElement element (. throwable getStackTrace)
             :when (. "clopad.txt" equals (. element getFileName))]
         element)
    (into-array StackTraceElement)
    (. throwable setStackTrace))
  throwable)

(defn- register-test [v, inputs->output-map, do-report]
  (alter-meta! v assoc :test
    #(doseq [[inputs output] inputs->output-map]
       (try
         (let [actual (apply @v inputs)]
           (do-report {:type     (if (= output actual) :pass :fail)
                       :message  (str "  inputs: " inputs)
                       :expected output
                       :actual   actual}))
         (catch Throwable throwable
           (do-report {:type     :error
                       :message  (str "  inputs: " inputs)
                       :expected output
                       :actual   (filter-stack-trace! throwable)}))))))

(defmacro defpure
  "Defines a pure function, illustrated by an exemplary inputs->output map"
  [name, inputs->output-map & rest]
  `(do
     (defn ~name ~@rest)
     (register-test (var ~name) ~inputs->output-map #(do-report %))))



(defpure parse-valley {["#.######
                        |#>>.<^<#
                        |#.<..<<#
                        |#>v.><>#
                        |#<^v^^>#
                        |######.#"] [">>.<^<"
                                     ".<..<<"
                                     ">v.><>"
                                     "<^v^^>"]}
  [^String valley]
  (vec (re-seq #"[.<>^v]{2,}" valley)))


(defpure plus {[[10 20]
                [ 1  2]] [11 22]}
  [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])


(def stay  [ 0  0])
(def west  [-1  0])
(def east  [+1  0])
(def north [ 0 -1])
(def south [ 0 +1])

(def all-directions [stay west east north south])


(defn legal-directions [width height]
  (into {[     0          -1      ] [stay south]                 ; start
         [     0           0      ] [stay east north south]      ; top left
         [(dec width)      0      ] [stay west south]            ; top right
         [     0      (dec height)] [stay east north]            ; bottom left
         [(dec width) (dec height)] [stay west north south]      ; bottom right
         [(dec width)      height ] [stay north]}                ; goal
    (concat 
      (for [x (range 1 (dec width))]
        {[     x           0      ] [stay west east south]       ; top
         [     x      (dec height)] [stay west east north]})     ; bottom)
      
      (for [y (range 1 (dec height))]
        {[     0           y      ] [stay east north south]      ; left
         [(dec width)      y      ] [stay west north south]})))) ; right


(defn next-positions [position directions width height valley minute]
  (for [dir (get directions position all-directions)
        :let [[x y :as xy] (plus position dir)]
        :when (or
                (= -1 y)
                (= height y)
                (not (or
                       (= \< (nth (valley y) (mod (+ x minute) width)))
                       (= \> (nth (valley y) (mod (- x minute) width)))
                       (= \^ (nth (valley (mod (+ y minute) height)) x))
                       (= \v (nth (valley (mod (- y minute) height)) x)))))]
    xy))


(defn explore [valley width height directions start goal minute]
  (loop [positions #{start}
         minute    minute]
    (if (contains? positions goal)
      minute
      (let [minute (inc minute)]
        (recur
          (into #{}
            (mapcat #(next-positions % directions width height valley minute))
            positions)
          minute)))))


(defpure part1 {[[">>.<^<"
                  ".<..<<"
                  ">v.><>"
                  "<^v^^>"]] 18}
  "What is the fewest number of minutes required
  to avoid the blizzards and reach the goal?"
  [valley]
  (let [height (count valley)
        width  (count (valley 0))
        dirs   (legal-directions width height)
        start  [     0      -1]
        goal   [(dec width) height]]
    
    (explore valley width height dirs start goal 0)))



(defpure part2 {[[">>.<^<"
                  ".<..<<"
                  ">v.><>"
                  "<^v^^>"]] (+ 18 23 13)}
  "What is the fewest number of minutes required to reach the goal,
  go back to the start, then reach the goal again?"
  [valley]
  (let [height (count valley)
        width  (count (valley 0))
        dirs   (legal-directions width height)
        start  [     0      -1]
        goal   [(dec width) height]]
    (->> 0
      (explore valley width height dirs start goal)
      (explore valley width height dirs goal start)
      (explore valley width height dirs start goal))))



(run-tests)
