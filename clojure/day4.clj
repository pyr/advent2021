(ns day4
  "subject: https://adventofcode.com/2021/day/4"
  (:require [clojure.java.io :as io]
            [clojure.string  :as str]))

(defn parse-board
  [[_ & lines]]
  {:cells
   (vec
    (for [[i x] (map-indexed vector  (mapcat #(str/split (str/trim %) #" +") lines))]
      {:val (Long/parseLong (str x))
       :pos i}))})

(defn parse-boards
  [lines]
  {:input  (mapv #(Long/parseLong (str %)) (str/split (first lines) #","))
   :boards (mapv parse-board (partition 6 (rest lines)))})

(defn mark-cells
  [cells v]
  (for [cell cells]
    (cond-> cell (= (:val cell) v) (assoc :mark! true))))

(defn won-row?
  [cells pos]
  (every? true?
          (for [cell cells
                :when (= (quot pos 5) (quot (:pos cell) 5))]
            (:mark! cell))))

(defn won-col?
  [cells pos]
  (every? true?
          (for [cell cells
                :when (= (rem pos 5) (rem (:pos cell) 5))]
            (:mark! cell))))

(defn score
  [cells v]
  (* v (reduce + 0 (map :val (remove :mark! cells)))))

(defn mark-and-check-board1
  [{:keys [won?] :as board} v]
  (let [cells (mark-cells (:cells board) v)
        pos   (:pos (first (filter #(= v (:val %)) cells)))]
    (cond-> (assoc board :cells cells)
      (and (some? pos)
           (not won?)
           (or (won-row? cells pos) (won-col? cells pos)))
      (assoc :won? true :score (score cells v)))))

(defn mark-boards1
  [acc v]
  (let [boards (mapv #(mark-and-check-board1 % v) (:boards acc))
        winner (first (filter :won? boards))]
    (if (some? winner)
      (reduced (:score winner))
      (assoc acc :boards boards))))

(defn solve1
  [{:keys [input] :as acc}]
  (reduce mark-boards1 (dissoc acc :input) input))

(defn mark-and-check-board2
  [{:keys [won?] :as board} v i]
  (let [cells (mark-cells (:cells board) v)
        pos   (:pos (first (filter #(= v (:val %)) cells)))]
    (cond-> (assoc board :cells cells)
      (and (some? pos)
           (not won?)
           (or (won-row? cells pos) (won-col? cells pos)))
      (assoc :won? true :when i :score (score cells v)))))

(defn mark-boards2
  [acc [i v]]
  (assoc acc :boards (mapv #(mark-and-check-board2 % v i) (:boards acc))))

(defn solve2
  [{:keys [input] :as acc}]
  (let [boards (->> (map-indexed vector input)
                    (reduce mark-boards2 (dissoc acc :input))
                    (:boards))
        winner (last (sort-by :when (filter :won? boards)))]
    (:score winner)))

(defn part1
  [path]
  (->> (io/resource path)
       (io/reader)
       (line-seq)
       (parse-boards)
       (solve1)
       (unreduced)))

(defn part2
  [path]
  (->> (io/resource path)
       (io/reader)
       (line-seq)
       (parse-boards)
       (solve2)
       (unreduced)))

(comment
  (part1 "input4.txt")
  (part2 "input4.txt")
  )
