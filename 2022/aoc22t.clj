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



(def example
  "1
  2
  -3
  3
  -2
  0
  4")



(defpure parse-numbers
  {[example] [1 2 -3 3 -2 0 4]}
  [^String input]
  (mapv parse-long (re-seq #"-?\d+" input)))



; struct Node {
;     struct Node* prev;
;     struct Node* next;
;     long long payload;
; } circle[N];



(defpure initial-prevs
  ;     0  1 2 3 4 5 6
  {[7] [6  0 1 2 3 4 5]}
  [^long n]
  (map #(mod (dec %) n) (range n)))



(defpure initial-nexts
  ;     0 1 2 3 4 5  6
  {[7] [1 2 3 4 5 6  0]}
  [^long n]
  (map #(mod (inc %) n) (range n)))



;          Part1                                    1             1
(defn both-parts [^String input, ^long decryption-key, ^long rounds]
  ;        Part2                            811589153            10
  (let [numbers (parse-numbers input)
        N       (count numbers)
        prevs   (int-array (initial-prevs N))
        nexts   (int-array (initial-nexts N))]
    
    (dotimes [_ rounds]
      ; Move each number forward or backward in the file a number of positions
      ; equal to the value of the number being moved. The list is circular.
      ; The numbers should be moved in the order they originally appear.
      (dotimes [i N]
        ; l<-->i<-->r
        (let [l (aget prevs i)
              r (aget nexts i)]
          
          ; l<-->i<-->r   unlink i
          ; l<------->r
          (aset-int nexts l r) ; l->r
          (aset-int prevs r l) ; l<-r
          
          ; find new neighbours
          (loop [l     l
                 r     r
                 steps (mod (* (numbers i) decryption-key) (dec N))]
            
            (if (pos? steps)
              (recur
                ; l<-->r
                ;      l<-->r
                r
                (aget nexts r)
                (dec steps))
              
              ; l<------->r
              ; l<-->i<-->r   relink i
              (do
                (aset-int nexts l i) ; l->i
                (aset-int prevs i l) ; l<-i
                (aset-int nexts i r) ; i->r
                (aset-int prevs r i) ; i<-r
                ))))))
    
    ; The grove coordinates can be found by looking at the 1000th, 2000th, and
    ; 3000th numbers after the value 0, wrapping around the list as necessary.
    (let [[_ x y z] (take-nth 1000
                      (map #(* (numbers %) decryption-key)
                        (iterate #(aget nexts %) (. numbers indexOf 0))))]
      ; What is the sum of the three numbers that form the grove coordinates?
      (+ x y z))))



(defpure part1
  {[example] 3}
  [^String input]
  (both-parts input 1 1))



(defpure part2
  {[example] 1623178306}
  [^String input]
  (both-parts input 811589153 10))



(run-tests)
