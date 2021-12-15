(ns day15
  (:require [clojure.java.io :as io]))

(defn- parse-row
  [[y row]]
  (for [[x c] (map-indexed vector row)]
    [[x y] (- (int c) (int \0))]))

(defn input
  [path]
  (let [lines (-> path io/resource io/reader line-seq)
        size  (count (first lines))]
    {:size    size
     :target  [(dec size) (dec size)]
     :grid    (into {}
                    (comp (map-indexed vector)
                          (mapcat parse-row))
                    lines)}))
(defn dijkstra
  [adjacents]
  (let [minpair #(first (sort-by val %))]
    (loop [q    {[0 0] 0}
           seen {}]
      (if-let [[pos cost] (minpair q)]
        (recur (reduce #(update %1 (key %2) (fnil min Long/MAX_VALUE) (+ cost (val %2)))
                       (dissoc q pos)
                       (eduction (remove #(contains? seen (key %))) (adjacents pos)))
               (assoc seen pos cost))
        seen))))

(defn adjacents-fn
  [{:keys [size grid]}]
  (fn [[x y]]
    (let [invalid? #(or (= % [0 0]) (some neg? %) (some (partial <= size) %))]
      (into {}
            (comp
             (remove invalid?)
             (map (juxt identity (partial get grid))))
            [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]))))

(defn part1
  [path]
  (let [board (input path)]
    (get (dijkstra (adjacents-fn board)) (:target board))))

(defn expand-cells
  [grid size]
  (let [increments (map (partial * size) (range 5))]
    (for [x    increments
          y    increments
          xoff (range size)
          yoff (range size)]
      [[(+ x xoff) (+ y yoff)]
       (let [v (+ (get grid [xoff yoff]) (quot x size) (quot y size))]
         (cond-> v (> v 9) (- 9)))])))

(defn expand-board
  [{:keys [size] :as board}]
  (let [size (* size 5)]
    (-> board
        (assoc :size size)
        (assoc :target [(dec size) (dec size)])
        (assoc :grid (into {} (expand-cells (:grid board) (:size board)))))))

(defn part2
  [path]
  (let [board (expand-board (input path))]
    (get (dijkstra (adjacents-fn board)) (:target board))))

(comment

  (time
   (part1 "input15.txt"))

  ;; A bit slow (~ 50s minute on my machine, 20s with Xmx8192)
  (time
   (part2 "input15.txt"))


  )
