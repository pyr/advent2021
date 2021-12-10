(ns day9
  (:require [clojure.java.io :as io]))

(defn adjacents-fn
  [lines]
  (let [max-x (count (first lines))
        max-y (count lines)]
    (fn [[x y]]
      (let [valid?    (fn [[x y]] (and (<= 0 x (dec max-x)) (<= 0 y (dec max-y))))
            adjacents [[(dec x) y] [(inc x) y] [x (dec y)][x (inc y)]]]
        (filter valid? adjacents)))))

(defn as-positions
  [[y heights]]
  (for [[x h] (map-indexed vector (seq heights))]
    [[x y] (- (int h) (int \0))]))

(defn input
  [path]
  (->> path
       io/resource
       io/reader
       line-seq))

(defn low-point?
  [heights height positions]
  (->> positions
       (map (partial get heights))
       (every? (partial < height))))

(defn height-map
  [lines]
  (->> lines
       (map-indexed vector)
       (mapcat as-positions)
       (into (sorted-map))))

(defn low-points
  [adjacents heights]
  (filter (fn [[pos h]] (low-point? heights h (adjacents pos))) heights))

(defn part1
  [path]
  (let [lines     (input path)
        adjacents (adjacents-fn lines)
        heights   (height-map lines)]
    (->>  heights
          (low-points adjacents)
          (map (comp inc val))
          (reduce +))))

(defn explore-basin
  [heights adjacents pos]
  (let [positions (adjacents pos)
        eligible? #(< (get heights pos) (get heights %) 9)]
    (->> positions
         (filter eligible?)
         (mapcat (partial explore-basin heights adjacents))
         (reduce conj #{pos}))))

(defn part2
  [path]
  (let [lines      (input path)
        adjacents  (adjacents-fn lines)
        heights    (height-map lines)]
    (->> heights
         (low-points adjacents)
         (map key)
         (map (partial explore-basin heights adjacents))
         (map count)
         (sort >)
         (take 3)
         (reduce *))))

(comment
  (part1 "input9.txt")
  (part2 "input9.txt")

  )
