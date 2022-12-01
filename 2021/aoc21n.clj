(ns user
  (:require [clojure.string :as string]
            [clojure.test :refer [is run-tests]]))

(defmacro examples [f & coll-arguments-result]
  `(fn []
     ~@(for [[arguments result] (partition 2 coll-arguments-result)]
         `(is (= ~(cons f arguments) ~result)))))

(def demo-polymer
  {:template "NNCB"
   :rules {"CH" \B
           "HH" \N
           "CB" \H
           "NH" \C
           "HB" \C
           "HC" \B
           "HN" \C
           "NN" \C
           "BH" \H
           "NC" \B
           "NB" \B
           "BN" \B
           "BB" \N
           "BC" \B
           "CC" \N
           "CN" \C}})

(defn cook-template
  {:test (examples cook-template ["ABCD"] {\A 1, \B 1, \C 1, \D 1,
                                           "AB" 1, "BC" 1, "CD" 1})}
  [^String template]
  (apply merge-with +
    (frequencies template)
    (for [i (range (dec (count template)))]
      {(. template substring i (+ i 2)) 1})))

(defn cook-rules
  {:test (examples cook-rules [{"AC" \B, "XZ" \Y}]
           {"AC" ["AB" "BC"], "XZ" ["XY" "YZ"]})}
  [rules]
  (apply merge-with +
    (for [[[prefix suffix :as pair] insert] rules]
      {pair [(str prefix insert) (str insert suffix)]})))

(defn insert
  {:test (let [rules (cook-rules (demo-polymer :rules))]
           (examples insert
             [rules (cook-template "NNCB")] (cook-template "NCNBCHB")
             [rules (cook-template "NCNBCHB")] (cook-template "NBCCNBBBCBHCB")
             [rules (cook-template "NBCCNBBBCBHCB")] (cook-template "NBBBCNCCNBBNBNBBCHBHHBCHB")
             [rules (cook-template "NBBBCNCCNBBNBNBBCHBHHBCHB")] (cook-template "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB")))}
  [rules template]
  (into {}
    (filter (fn [[key value]] (pos? value))
      (apply merge-with + template
        (for [[pair n] template
              :when (string? pair)
              :let [[left right] (rules pair)]]
          (merge-with + {pair (- n)} {left n} {right n} {(. right charAt 0) n}))))))

(defn both-parts
  {:test (examples both-parts
           [demo-polymer 10] 1588
           [demo-polymer 40] 2188189693529)}
  [{template :template, rules :rules} steps]
  (let [occs (->> (cook-template template)
               (iterate (partial insert (cook-rules rules)))
               (drop steps)
               first
               (filter (fn [[key value]] (char? key)))
               vals)
        least (apply min occs)
        most  (apply max occs)]
    (- most least)))

(run-tests)
