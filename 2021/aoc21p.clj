(ns user
  (:require [clojure.string :as string]
            [clojure.test :refer [is run-tests]]))

(defmacro examples [f & coll-arguments-result]
  `(fn []
     ~@(for [[arguments result] (partition 2 coll-arguments-result)]
         `(is (= ~(cons f arguments) ~result)))))

(defn binary
  "Convert the hexadecimal representation into binary"
  {:test (examples binary
           ["D2FE28"] (seq "110100101111111000101000"))}
  [hex]
  (mapcat
    (fn [ch] (["0000" "0001" "0010" "0011" "0100" "0101" "0110" "0111"
               "1000" "1001" "1010" "1011" "1100" "1101" "1110" "1111"]
               (Character/digit ch 16)))
    hex))

(defn integer
  "Parses a fixed-width integer"
  {:test (examples integer
           ["10011010010rest" 11] [1234 (seq "rest")])}
  [bits, ^long width]
  [(reduce
     (fn [result item]
       (+ (* result 2) (Character/digit item 2)))
     0
     (take width bits))
   (drop width bits)])

(defn literal
  "Literal value packets encode a single binary number"
  {:test (examples literal
           ["101111111000101rest"] [2021 (seq "rest")])}
  [bits]
  (loop [value         0
         [more & bits] bits]
    (let [[digit bits] (integer bits 4)
          value        (+ (* value 16) digit)]
      ; Each group is prefixed by a 1 bit
      ; except the last group, which is prefixed by a 0 bit
      (if (= \0 more)
        [value bits]
        (recur value bits)))))

(defn packet
  {:test (examples packet
           ["110100101111111000101rest"]
           [{:version 6, :type 4, :value 2021} (seq "rest")]
           
           ["0011100000000000011011110100010100101001000100100rest"]
           [{:version 1, :type 6, :operands
             [{:version 6, :type 4, :value 10}
              {:version 2, :type 4, :value 20}]} (seq "rest")])}
  [bits]
  ; the first three bits encode the packet version,
  ; and the next three bits encode the packet type ID.
  (let [[version bits] (integer bits 3)
        [type    bits] (integer bits 3)]
    (if (= 4 type)
      ; Packets with type ID 4 represent a literal value
      (let [[value bits] (literal bits)]
        [{:version version, :type type, :value value} bits])
      ; Every other type of packet represent an operator
      ; with one or more sub-packets contained within
      (let [[length-type & bits] bits]
        (if (= \1 length-type)
          ; The next 11 bits are a number that represents
          ; the number of sub-packets immediately contained
          (loop [operands      []
                 [length bits] (integer bits 11)]
            (if (zero? length)
              [{:version version, :type type, :operands operands}
               bits]
              (let [[operand bits] (packet bits)]
                (recur (conj operands operand) [(dec length) bits]))))
          ; The next 15 bits are a number that represents
          ; the total length in bits of the sub-packets
          (let [[length bits] (integer bits 15)
                continue-with (drop length bits)]
            (loop [operands []
                   bits     (take length bits)]
              (if (empty? bits)
                [{:version version, :type type, :operands operands}
                 continue-with]
                (let [[operand bits] (packet bits)]
                  (recur (conj operands operand) bits))))))))))

(defn version-sum [{version :version, operands :operands}]
  (transduce
    (map version-sum)
    +
    version
    operands))

(defn part-1
  "Parse the hierarchy of the packets throughout the transmission
  and add up all of the version numbers"
  {:test (examples part-1
           ["8A004A801A8002F478"] 16
           ["620080001611562C8802118E34"] 12
           ["C0015000016115A2E0802F182340"] 23
           ["A0016C880162017C3686B18A3D4780"] 31)}
  [^String hex]
  (-> hex
    binary
    packet
    first
    version-sum))

(defn boolean->int [b]
  (if b 1 0))

(defn evaluate [{type :type, value :value,
                 [a b :as operands] :operands}]
  (case type
    4 value
    
    0 (transduce (map evaluate)  +      0          operands) ; sum
    1 (transduce (map evaluate)  *      1          operands) ; product
    2 (transduce (map evaluate) min Long/MAX_VALUE operands) ; minimum
    3 (transduce (map evaluate) max Long/MIN_VALUE operands) ; maximum
    
    5 (boolean->int (> (evaluate a) (evaluate b)))   ; greater than
    6 (boolean->int (< (evaluate a) (evaluate b)))   ; less than
    7 (boolean->int (= (evaluate a) (evaluate b))))) ; equal to

(defn part-2
  "Work out the value of the outermost packet"
  {:test (examples part-2
           ["C200B40A82"] 3
           ["04005AC33890"] 54
           ["880086C3E88112"] 7
           ["CE00C43D881120"] 9
           ["D8005AC2A8F0"] 1
           ["F600BC2D8F"] 0
           ["9C005AC2F8F0"] 0
           ["9C0141080250320F1802104A08"] 1)}
  [^String hex]
  (-> hex
    binary
    packet
    first
    evaluate))

(run-tests)
