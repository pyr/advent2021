(ns day1
  "subject: https://adventofcode.com/2021/day/1"
  (:require [clojure.java.io :as io]))

(defn part1
  [path]
  (->> (io/resource path)
       (io/reader)
       (line-seq)
       (map #(Long/parseLong %))
       (partition 2 1)
       (map #(- (last %) (first %)))
       (filter pos?)
       (count)))

(defn part2
  [path]
  (->> (io/resource path)
       (io/reader)
       (line-seq)
       (map #(Long/parseLong %))
       (partition 3 1)
       (map #(apply + %))
       (partition 2 1)
       (map #(- (last %) (first %)))
       (filter pos?)
       (count)))

(comment
  (part1 "input1.txt")
  (part2 "input1.txt")
)
