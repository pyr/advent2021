(ns day11
  (:require [clojure.java.io :as io]
            [clojure.set     :as set]))

(defn input
  [path]
  (->> path
       io/resource
       io/reader
       line-seq))

(defn adjacents
  [[base-x base-y :as base-pos]]
  (for [x ((juxt dec identity inc) base-x)
        y ((juxt dec identity inc) base-y)
        :let [pos [x y]]
        :when (and (not= base-pos pos)
                   (<= 0 x 9)
                   (<= 0 y 9))]
    pos))

(defn grid-line
  [[y levels]]
  (for [[x h] (map-indexed vector (seq levels))]
    [[x y] (- (int h) (int \0))]))

(defn grid
  [lines]
  (map-indexed vector (map grid-line lines))
  (->> lines
       (map-indexed vector)
       (mapcat grid-line)
       (into {})))

(defn process-flash
  [grid pos]
  (reduce #(update %1 %2 inc) grid (adjacents pos)))

(defn find-flashes
  [grid]
  (for [[pos level] grid :when (> level 9)] pos))

(defn zero-flashes
  [grid]
  (reduce-kv (fn [m k v] (assoc m k (if (> v 9) 0 v))) {} grid))

(defn count-flashes
  [grid]
  (reduce-kv (fn [acc _ v] (cond-> acc (> v 9) inc)) 0 grid))

(defn increment-position
  [grid pos]
  (update grid pos inc))

(defn apply-step
  [[grid flashcount] _]
  (loop [grid      (reduce increment-position grid (keys grid))
         flashed   #{}]
    (if-some [flashing (seq (set/difference (set (find-flashes grid)) (set flashed)))]
      (recur (reduce increment-position grid (mapcat adjacents flashing))
             (set (concat flashed flashing)))
      [(zero-flashes grid) (+ flashcount (count-flashes grid))])))

(defn count-flashes-for-steps
  [input]
  (last
   (reduce apply-step [(grid input) 0] (range 100))))

(defn part1
  [path]
  (count-flashes-for-steps (input path)))

(defn stop-at-first-synchronized
  [input]
  (loop [step 1
         [grid] (apply-step [(grid input) 0] 0)]
    (if (every? zero? (vals grid))
      step
      (recur (inc step) (apply-step [grid 0] 0)))))

(defn part2
  [path]
  (stop-at-first-synchronized (input path)))

(comment

  (part1 "input11.txt")
  (part2 "input11.txt")
  )
