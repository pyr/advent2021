(ns day16
  (:require [clojure.java.io :as io])
  (:import java.math.BigInteger))

(defn input
  [path]
  (-> path io/resource io/reader line-seq first))

(defn parse-header
  [b idx]
  [(-> (.shiftRight b (- (.bitLength b) idx 3))
       (.and (BigInteger/valueOf 7)))
   (-> (.shiftRight b (- (.bitLength b) idx 6))
       (.and (BigInteger/valueOf 7)))])

(defn round8
  [n]
  (if (zero? (rem n 8))
    n
    (* 8 (inc (quot n 8)))))

(defn parse-literal
  [b idx]
  (loop [idx   idx
         group nil]
    (let [word  (-> (.shiftRight b  (- (.bitLength b) idx 5))
                    (.and (BigInteger/valueOf 31)))
          group (conj group (.byteValueExact (.and word (BigInteger/valueOf 0xf))))]
      (if (.testBit word 4)
        (recur (+ idx 5) group)
        [(first (reduce (fn [[acc shift] x]
                          [(bit-or acc (bit-shift-left x (* shift 4))) (inc shift)])
                        [0 0]
                        group))
         (+ idx 5)]))))

(declare dispatch-parse)

(defmulti subpackets
  (fn [b idx]
    (-> (.shiftRight b (dec (Math/abs (- idx (.bitLength b)))))
        (.and (BigInteger/valueOf 0x01))
        (pos?))))

(defmethod subpackets true
  [b idx]
  (let [packet-count (-> (.shiftRight b (- (.bitLength b) idx 12))
                         (.and (BigInteger/valueOf 0x7ff)))]
    (loop [idx     (+ idx 12)
           packets []]
      (if (< (count packets) packet-count)
        (let [{:keys [idx] :as p} (dispatch-parse b idx)]
          (recur idx (conj packets p)))
        [packets idx]))))

(defmethod subpackets false
  [b idx]
  (let [packet-len (-> (.shiftRight b (- (.bitLength b) idx 16))
                       (.and (BigInteger/valueOf 0x3fff)))
        limit      (+ idx 16 packet-len)]
    (loop [idx     (+ idx 16)
           packets []]
      (if (< idx limit)
        (let [{:keys [idx] :as p} (dispatch-parse b idx)]
          (recur idx (conj packets p)))
        [packets idx]))))

(defn dispatch-parse
  [b idx]
  (let [[version type] (parse-header b idx)
        next-idx       (+ idx 6)]
    (cond
      (= type 4)
      (let [[lit idx] (parse-literal b next-idx)]
        {:type    :literal
         :version version
         :value   lit
         :idx     idx})
      :else
      (let [[packets idx] (subpackets b next-idx)]
        {:type    (case type
                    0 :sum
                    1 :product
                    2 :min
                    3 :max
                    5 :gt
                    6 :lt
                    7 :eq)
         :highbit (.testBit b next-idx)
         :version version
         :idx     idx
         :packets packets}))))

(defn parse
  ([s radix]
   (let [b   (BigInteger. s radix)
         idx (- (.bitLength b) (round8 (.bitLength b)))]
     (dispatch-parse b idx)))
  ([s]
   (parse s 16)))

(defn version-count
  [{:keys [packets version]}]
  (long
   (+ version (if (some? packets)
                (reduce + (map version-count packets))
                0))))

(defmulti process-packet :type)

(defmethod process-packet :sum
  [{:keys [packets]}]
  (reduce + (map process-packet packets)))

(defmethod process-packet :product
  [{:keys [packets]}]
  (reduce * (map process-packet packets)))

(defmethod process-packet :min
  [{:keys [packets]}]
  (reduce min Long/MAX_VALUE (map process-packet packets)))

(defmethod process-packet :max
  [{:keys [packets]}]
  (reduce max 0 (map process-packet packets)))

(defmethod process-packet :gt
  [{:keys [packets]}]
  (if (> (process-packet (first packets))
         (process-packet (last packets)))
    1
    0))

(defmethod process-packet :lt
  [{:keys [packets]}]
  (if (< (process-packet (first packets))
         (process-packet (last packets)))
    1
    0))

(defmethod process-packet :eq
  [{:keys [packets]}]
  (if (= (process-packet (first packets))
         (process-packet (last packets)))
    1
    0))

(defmethod process-packet :literal
  [{:keys [value]}]
  value)

(defn part1
  [path]
  (-> path input parse version-count))

(defn part2
  [path]
  (-> path input parse process-packet))

(comment

  (version-count
   (parse "A0016C880162017C3686B18A3D4780"))
  (version-count
   (parse "C0015000016115A2E0802F182340"))
  (version-count
   (parse "8A004A801A8002F478"))

  (part1 "input16.txt")
  (part2 "input16.txt"))
