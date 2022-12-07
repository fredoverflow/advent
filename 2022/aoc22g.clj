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



(defpure parse-fs
  {[example] {"a"
              {"e"
               {"i"        584},
               "f"       29116,
               "g"        2557,
               "h.lst"   62596},
              "b.txt" 14848514,
              "c.dat"  8504156,
              "d"
              {"j"     4060174,
               "d.log" 8033020,
               "d.ext" 5626152,
               "k"     7214296}}}
  [^String input]
  (loop [fs   {}
         path [] ; ["a" "e"]
         [line & lines] (string/split input #"\n *")]
    
    (if-not line
      fs
      (let [[        _      ls      dir   size file         cd]
            (re-find #"[$] (ls)|dir (.+)|(\d+) (.+)|[$] cd (.+)" line)]
        (cond
          ls   (recur fs path lines)
          ;                              ["a" "e"]
          dir  (recur (assoc-in fs (conj path dir ) {}               ) path lines)
          ;                              ["a" "b.txt"]      14848514
          file (recur (assoc-in fs (conj path file) (parse-long size)) path lines)
          
          cd   (case cd
                 
                 "/"  (recur fs []         lines)
                 
                 ".." (recur fs (pop path) lines)
                 
                 (recur fs (conj path cd)  lines)))))))



(defpure path->size
  {[(parse-fs example)] {[]    48381165
                         ["a"]    94853
                         ["a" "e"]  584
                         ["d"] 24933642}}
  "The total size of a directory is the sum of the sizes
  of the files it contains, directly or indirectly."
  [fs]
  (let [result  (new java.util.HashMap)
        
        collect (fn collect [path dir]
                  (let [size (transduce
                               (map (fn [[name size-or-dir]]
                                      (if (int? size-or-dir)
                                        size-or-dir
                                        (collect (conj path name) size-or-dir))))
                               +
                               0
                               dir)]
                    (. result put path size)
                    size))]
    
    (collect [] fs)
    (into {} result)))



(defpure part1
  {[example] 95437}
  "Find all of the directories with a total size of at most 100000.
  What is the sum of the total sizes of those directories?"
  [^String input]
  (let [fs   (parse-fs input)
        p->s (path->size fs)]
    
    (transduce
      (comp
        (map val)
        (filter #(<= % 100000)))
      +
      0
      p->s)))



(defpure part2
  {[example] 24933642}
  "Find the smallest directory that, if deleted, would free up enough space on
  the filesystem to run the update. What is the total size of that directory?"
  [^String input]
  (let [fs   (parse-fs input)
        p->s (path->size fs)

        used (p->s [])
        debt (- used 40000000)]
    
    (transduce
      (comp
        (map val)
        (filter #(>= % debt)))
      min
      Long/MAX_VALUE
      p->s)))



(run-tests)
