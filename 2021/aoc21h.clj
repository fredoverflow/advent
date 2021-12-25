(ns user
  (:require [clojure.string :as string]
            [clojure.test :refer [is run-tests]]))

(defmacro examples [f & coll-arguments-result]
  `(fn []
     ~@(for [[arguments result] (partition 2 coll-arguments-result)]
         `(is (= ~(cons f arguments) ~result)))))

(def demo-entries
  [[["be" "cfbegad" "cbdgef" "fgaecd" "cgeb" "fdcge" "agebfd" "fecdb" "fabcd" "edb"]
    ["fdgacbe" "cefdb" "cefbgd" "gcbe"]]

   [["edbfga" "begcd" "cbg" "gc" "gcadebf" "fbgde" "acbgfd" "abcde" "gfcbed" "gfec"]
    ["fcgedb" "cgb" "dgebacf" "gc"]]

   [["fgaebd" "cg" "bdaec" "gdafb" "agbcfd" "gdcbef" "bgcad" "gfac" "gcb" "cdgabef"]
    ["cg" "cg" "fdcagb" "cbg"]]

   [["fbegcd" "cbd" "adcefb" "dageb" "afcb" "bc" "aefdc" "ecdab" "fgdeca" "fcdbega"]
    ["efabcd" "cedba" "gadfec" "cb"]]

   [["aecbfdg" "fbg" "gf" "bafeg" "dbefa" "fcge" "gcbea" "fcaegb" "dgceab" "fcbdga"]
    ["gecf" "egdcabf" "bgf" "bfgea"]]

   [["fgeab" "ca" "afcebg" "bdacfeg" "cfaedg" "gcfdb" "baec" "bfadeg" "bafgc" "acf"]
    ["gebdcfa" "ecba" "ca" "fadegcb"]]

   [["dbcfg" "fgd" "bdegcaf" "fgec" "aegbdf" "ecdfab" "fbedc" "dacgb" "gdcebf" "gf"]
    ["cefg" "dcbef" "fcge" "gbcadfe"]]

   [["bdfegc" "cbegaf" "gecbf" "dfcage" "bdacg" "ed" "bedf" "ced" "adcbefg" "gebcd"]
    ["ed" "bcgafe" "cdgba" "cbgef"]]

   [["egadfb" "cdbfeg" "cegd" "fecab" "cgb" "gbdefca" "cg" "fgcdab" "egfdb" "bfceg"]
    ["gbdfcae" "bgc" "cg" "cgb"]]

   [["gcafb" "gcf" "dcaebfg" "ecagb" "gf" "abcdeg" "gaef" "cafbge" "fdbac" "fegbdc"]
    ["fgae" "cfgab" "fg" "bagce"]]])

(defn part-1
  {:test (examples part-1 [demo-entries] 26)}
  [entries]
  (count
    (for [[patterns digits] entries
          digit             digits
          ;        1 4 7 8
          :when (#{2 4 3 7} (count digit))]
      digit)))

(defn digit->binary
  {:test (examples digit->binary
           [""] 0
           ["a"] 2
           ["b"] 4
           ["c"] 8
           ["d"] 16
           ["e"] 32
           ["f"] 64
           ["g"] 128
           ["ab"] 6
           ["ba"] 6
           ["abcdefg"] 254)}
  ^long [^String digit]
  (reduce
    (fn [result segment]
      (->> segment
        long
        (bit-and 7)
        (bit-shift-left 1)
        (bit-or result)))
    0
    digit))

(defn difference
  (^long [^long a, ^long b]
    (bit-and a (bit-not b)))
  
  (^long [^long a, ^long b, ^long c]
    (bit-and a (bit-not b) (bit-not c)))
  
  (^long [^long a, ^long b, ^long c, ^long d]
    (bit-and a (bit-not b) (bit-not c) (bit-not d))))

(def adg     (digit->binary "adg"))
(def abfg    (digit->binary "abfg"))
(def abcdefg (digit->binary "abcdefg"))

(defn decoder [patterns]
  (let [by-segment-count (group-by
                           #(Long/bitCount %)
                           (map digit->binary patterns))
        
        [cf] (by-segment-count 2)
        [acf] (by-segment-count 3)
        a (difference acf cf)
        
        [bcdf] (by-segment-count 4)
        bd (difference bcdf cf)
        
        adg (apply bit-and (by-segment-count 5))
        d (bit-and adg bd)
        g (difference adg a d)
        b (difference bd d)
        
        abfg (apply bit-and (by-segment-count 6))
        f (difference abfg a b g)
        c (difference cf f)
        e (difference abcdefg a bcdf g)]
    
    {(bit-or a b c   e f g) 0
     (bit-or     c     f  ) 1
     (bit-or a   c d e   g) 2
     (bit-or a   c d   f g) 3
     (bit-or   b c d   f  ) 4
     (bit-or a b   d   f g) 5
     (bit-or a b   d e f g) 6
     (bit-or a   c     f  ) 7
     (bit-or a b c d e f g) 8
     (bit-or a b c d   f g) 9}))

(defn decode-entry [[patterns digits]]
  (let [decode (decoder patterns)]
    (reduce
      (fn [result digit]
        (+ (* 10 result) (decode (digit->binary digit))))
      0
      digits)))

(defn part-2
  {:test (examples part-2 [demo-entries] 61229)}
  [entries]
  (reduce
    (fn [result entry]
      (+ result (decode-entry entry)))
    0
    entries))

(run-tests)
