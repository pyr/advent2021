(ns day5
  (:require [clojure.java.io :as io]))

(defn input
  [path]
  (for [line (-> path io/resource io/reader line-seq)]
    (->> (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" line)
         (rest)
         (mapv #(Long/parseLong %)))))

(defn row-or-col?
  [[x1 y1 x2 y2]]
  (or (= y1 y2)
      (= x1 x2)))

(defn diagonal-position
  [x1 y1 x2 y2]
  (let [x-op        (if (< x1 x2) inc dec)
        y-op        (if (< y1 y2) inc dec)
        boundary-op (if (< x1 x2) <= >=)]
    (loop [res []
          x   x1
          y   y1]
      (if (boundary-op x x2)
        (recur (conj res [x y]) (x-op x) (y-op y))
        res))))

(defn as-pos
  [[x1 y1 x2 y2]]
  (cond
    (= y1 y2) (for [x (range (min x1 x2) (inc (max x1 x2)))] [x y1])
    (= x1 x2) (for [y (range (min y1 y2) (inc (max y1 y2)))] [x1 y])
    :else     (diagonal-position x1 y1 x2 y2)))

(defn handle-input
  [m input]
  (update m input (fnil inc 0)))

(defn part1
  [path]
  (->> (input path)
       (filter row-or-col?)
       (mapcat as-pos)
       (reduce handle-input {})
       (filter #(>= (val %) 2))
       (count)))

(defn part2
  [path]
  (->> (input path)
       (mapcat as-pos)
       (reduce handle-input {})
       (filter #(>= (val %) 2))
       (count)))

(comment
  (part1 "input5.txt")
  (part2 "input5.txt")
  )
