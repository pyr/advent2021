(ns day2
  "subject: https://adventofcode.com/2021/day/2"
  (:require [clojure.java.io :as io]
            [clojure.string  :as str]))

(defn part1
  [path]
  (->> (io/resource path)
       (io/reader)
       (line-seq)
       (map (fn [line]
              (let [[cmd amount] (str/split line #" ")]
                [(keyword cmd) (Long/parseLong amount)])))
       (reduce (fn [[x y] [cmd amount]]
                 (case cmd
                   :forward [(+ x amount) y]
                   :up      [x (- y amount)]
                   :down    [x (+ y amount)]))
               [0 0])
       (apply *)))

(defn part2
  [path]
  (->> (io/resource path)
       (io/reader)
       (line-seq)
       (map (fn [line]
              (let [[cmd amount] (str/split line #" ")]
                [(keyword cmd) (Long/parseLong amount)])))
       (reduce (fn [[aim x depth] [cmd amount]]
                 (case cmd
                   :forward [aim (+ x amount) (+ depth (* aim amount))]
                   :up      [(- aim amount) x depth]
                   :down    [(+ aim amount) x depth]))
               [0 0 0])
       (drop 1)
       (apply *)))

(comment
  (part1 "input2.txt")
  (part2 "input2.txt"))
