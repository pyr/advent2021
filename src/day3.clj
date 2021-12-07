(ns day3
  "subject: https://adventofcode.com/2021/day/3"
  (:require [clojure.java.io :as io]))

(defn as-bit-seq
  [s]
  (vec
   (for [x (seq s)]
     (if (= \1 x) 1 0))))

(defn init-bits [width]
  (vec (repeat width 0)))

(defn to-gamma-and-espilon
  [line-count bits]
  (let [msb-lsb (for [b bits]
                  (if (< (- line-count b) (/ line-count 2))
                    [0 1]
                    [1 0]))]
    [(reduce str (map first msb-lsb))
     (reduce str (map last msb-lsb))]))

(defn get-group
  [msb? [x y] pos]
  (cond
    (= (count x) (count y))
    (condp = [(boolean msb?) (nth (first x) pos)]
      [true 0]  y
      [true 1]  x
      [false 0] x
      [false 1] y)

    (> (count x) (count y))
    (if msb? x y)

    :else
    (if msb? y x)))

(defn rating
  [bit-seqs rating-fn]
  (loop [numbers bit-seqs
         pos     0]
    (if (= 1 (count numbers))
      (Long/parseLong (reduce str (first numbers)) 2)
      (-> (partition-by #(nth % pos) numbers)
          (rating-fn pos)
          (recur (inc pos))))))

(defn input
  [path]
  (->> (io/resource path)
       (io/reader)
       (line-seq)))

(defn part1
  [path]
  (let [values (input path)
        width  (count (first values))
        line-count (count values)]
    (->> values
         (map as-bit-seq)
         (reduce #(mapv + %1 %2) (init-bits width))
         (to-gamma-and-espilon line-count)
         (map #(Long/parseLong % 2))
         (apply *))))

(defn part2
  [path]
  (let [bit-seqs (->> (input path) (map as-bit-seq) (sort))]
    (*
     (rating bit-seqs (partial get-group true))
     (rating bit-seqs (partial get-group false)))))

(comment
  (part1 "input3.txt")
  (part2 "input3.txt"))
