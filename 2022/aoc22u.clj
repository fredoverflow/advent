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



(def example "
  root: pppw + sjmn
  dbpl: 5
  cczh: sllz + lgvd
  zczc: 2
  ptdq: humn - dvpt
  dvpt: 3
  lfqf: 4
  humn: 5
  ljgn: 2
  sjmn: drzm * dbpl
  sllz: 4
  pppw: cczh / lfqf
  lgvd: ljgn * ptdq
  drzm: hmdt - zczc
  hmdt: 32")



(defpure parse-equations
  {[example] {"root" ["pppw" "+" "sjmn"]
              "dbpl" 5
              "cczh" ["sllz" "+" "lgvd"]
              "zczc" 2
              "ptdq" ["humn" "-" "dvpt"]
              "dvpt" 3
              "lfqf" 4
              "humn" 5
              "ljgn" 2
              "sjmn" ["drzm" "*" "dbpl"]
              "sllz" 4
              "pppw" ["cczh" "/" "lfqf"]
              "lgvd" ["ljgn" "*" "ptdq"]
              "drzm" ["hmdt" "-" "zczc"]
              "hmdt" 32}}
  [^String input]
  (into {}
    (for [[       _   a        num    b      op      c]
          (re-seq #"(\w+): (?:(\d+)|(\w+) ([-+*/]) (\w+))" input)]
      
      {a (if num
           (parse-long num)
           [b op c])})))



(defn evaluate [equations, ^String root]
  
  ((fn eval [^String a]
     (let [expression (equations a)]
       (if (number? expression)
         expression
         (let [[b op c] expression]
           (({"+" +
              "-" -
              "*" *
              "/" /} op) (eval b) (eval c))))))
    root))



(defpure part1
  {[example] 152}
  "What number will the monkey named root yell?"
  [^String input]
  (evaluate (parse-equations input) "root"))



;        ____
; ptdq = humn - dvpt
; humn = ptdq + dvpt

;               ____
; lgvd = ljgn * ptdq
; ptdq = lgvd / ljgn

;               ____
; cczh = sllz + lgvd
; lgvd = cczh - sllz

;        ____
; pppw = cczh / lfqf
; cczh = pppw * lfqf

;        ____
;        pppw = sjmn
; pppw = drzm * dbpl

(defn solve-for-humn
  [equations]
  (let [; The correct operation for monkey root should be =
        equations  (assoc-in equations ["root" 1] "=")
        
        containing (into {}
                     (comp
                       (remove (fn [[a expression]] (number? expression)))
                       #_{"cczh" ["sllz" "+" "lgvd"]
                          
                          "ptdq" ["humn" "-" "dvpt"]
                          ...... ...................}
                       (map (fn [[a [b op c] :as equation]] {b equation
                                                             c equation}))
                       #_{"sllz" ["cczh" ["sllz" "+" "lgvd"]]
                          "lgvd" ["cczh" ["sllz" "+" "lgvd"]]
                          
                          "humn" ["ptdq" ["humn" "-" "dvpt"]]
                          "dvpt" ["ptdq" ["humn" "-" "dvpt"]]
                          ...... ............................})
                     equations)]
    
    (loop [equations equations
           ; You need to figure out what number you need to yell
           ; so that root's equality check passes.
           solve-for "humn"]
      
      (let [[a [b op c]] (containing solve-for)]
        
        (if (= solve-for b)
          (case op
            "+" (recur (assoc equations b [a "-" c]) ; a = b + c    b = a - c
                  a)
            "-" (recur (assoc equations b [a "+" c]) ; a = b - c    b = a + c
                  a)
            "*" (recur (assoc equations b [a "/" c]) ; a = b * c    b = a / c
                  a)
            "/" (recur (assoc equations b [a "*" c]) ; a = b / c    b = a * c
                  a)
            "=" (assoc equations b (equations c)))   ; b = c
          
          ; solve for c
          (case op
            "+" (recur (assoc equations c [a "-" b]) ; a = b + c    c = a - b
                  a)
            "-" (recur (assoc equations c [b "-" a]) ; a = b - c    c = b - a
                  a)
            "*" (recur (assoc equations c [a "/" b]) ; a = b * c    c = a / b
                  a)
            "/" (recur (assoc equations c [b "/" a]) ; a = b / c    c = b / a
                  a)
            "=" (assoc equations c (equations b))))))))  ; b = c



(defpure part2
  {[example] 301}
  "What number do you yell to pass root's equality test?"
  [^String input]
  (evaluate (solve-for-humn (parse-equations input)) "humn"))



(run-tests)
