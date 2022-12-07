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
  "You browse around the filesystem to assess the situation"
  
  "$ cd /
  $ ls
  dir a
  14848514 b.txt
  8504156 c.dat
  dir d
  $ cd a
  $ ls
  dir e
  29116 f
  2557 g
  62596 h.lst
  $ cd e
  $ ls
  584 i
  $ cd ..
  $ cd ..
  $ cd d
  $ ls
  4060174 j
  8033020 d.log
  5626152 d.ext
  7214296 k")



(defpure measure-directories
  {[example] [584
              94853
              24933642
              48381165]}
  "The total size of a directory is the sum of the sizes
  of the files it contains, directly or indirectly."
  [^String input]
  (let [lines (new java.util.ArrayDeque (string/split input #"\n *"))
        sizes (new java.util.ArrayDeque)
        
        dir-size (fn dir-size [size-so-far]
                   
                   (if-not (. lines isEmpty)
                     
                     (let [[        _      ls-dir      size file         cd]
                           (re-find #"([$] ls|dir .+)|(\d+) (.+)|[$] cd (.+)"
                             (. lines pop))]
                       
                       (cond
                         ls-dir (recur size-so-far)
                         
                         file   (recur (+ size-so-far (parse-long size)))
                         
                         cd     (case cd
                                  
                                  ".." (do
                                         (. sizes add size-so-far)
                                         size-so-far)
                                  
                                  (recur (+ size-so-far (dir-size 0))))))
                     
                     (do
                       (. sizes add size-so-far)
                       size-so-far)))]
    
    (dir-size 0)
    (. sizes removeLast) ; initial "$ cd /" stays in root, adds 48381165 twice
    (into [] sizes)))



(defpure part1
  {[example] 95437}
  "Find all of the directories with a total size of at most 100000.
  What is the sum of the total sizes of those directories?"
  [^String input]
  (transduce
    (filter #(<= % 100000))
    +
    0
    (measure-directories input)))



(defpure part2
  {[example] 24933642}
  "Find the smallest directory that, if deleted, would free up enough space on
  the filesystem to run the update. What is the total size of that directory?"
  [^String input]
  (let [sizes (measure-directories input)
        used  (peek sizes)
        debt  (- used 40000000)]
    
    (transduce
      (filter #(>= % debt))
      min
      Long/MAX_VALUE
      sizes)))



(run-tests)
