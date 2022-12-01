(ns user
  (:require [clojure.string :as string]
            [clojure.test :refer [is run-tests]]))

(defmacro examples [f & coll-arguments-result]
  `(fn []
     ~@(for [[arguments result] (partition 2 coll-arguments-result)]
         `(is (= ~(cons f arguments) ~result)))))

(def demo-lines
  ["[({(<(())[]>[[{[]{<()<>>"
   "[(()[<>])]({[<{<<[]>>("
   "{([(<{}[<>[]}>{[]{[(<()>"
   "(((({<>}<{<{<>}{[]{[]{}"
   "[[<[([]))<([[{}[[()]]]"
   "[{[{({}]{}}([{[{{{}}([]"
   "{<[[]]>}<{[{[{[]{()[[[]"
   "[<(<(<(<{}))><([]([]()"
   "<{([([[(<>()){}]>(<<{{"
   "<{([{{}}[<[[[<>{}]]]>[]]"])

(defn parse [line, on-incomplete, on-corrupted]
  (loop [stack    ()
         [x & xs] line]
    (let [partner ({\( \), \[ \], \{ \}, \< \>} x)]
      (cond
        (nil? x)            (on-incomplete stack)
        partner             (recur (conj stack partner) xs)
        (= x (first stack)) (recur (rest stack)         xs)
        :else               (on-corrupted x)))))

(defn corrupted-score
  {:test (examples corrupted-score
           ["{([(<{}[<>[]}>{[]{[(<()>"] 1197
           ["[[<[([]))<([[{}[[()]]]"] 3
           ["[{[{({}]{}}([{[{{{}}([]"] 57
           ["[<(<(<(<{}))><([]([]()"] 3
           ["<{([([[(<>()){}]>(<<{{"] 25137)}
  [line]
  (parse line
    (constantly 0)
    {\) 3, \] 57, \} 1197, \> 25137}))

(defn part-1
  {:test (examples part-1 [demo-lines] 26397)}
  [lines]
  (transduce (map corrupted-score) + 0 lines))

(defn incomplete-score
  {:test (examples incomplete-score
           ["[({(<(())[]>[[{[]{<()<>>"] 288957
           ["[(()[<>])]({[<{<<[]>>("] 5566
           ["(((({<>}<{<{<>}{[]{[]{}"] 1480781
           ["{<[[]]>}<{[{[{[]{()[[[]"] 995444
           ["<{([{{}}[<[[[<>{}]]]>[]]"] 294)}
  [line]
  (parse line
    (fn [stack]
      (reduce
        (fn [result item]
          (+ (* 5 result) ({\) 1, \] 2, \} 3, \> 4} item)))
        0
        stack))
    (constantly 0)))

(defn part-2
  {:test (examples part-2 [demo-lines] 288957)}
  [lines]
  (let [sorted (->> lines
                 (map incomplete-score)
                 (filter pos?)
                 sort)
        middle (quot (count sorted) 2)]
    (nth sorted middle)))

(run-tests)
