(ns day12
  (:require [clojure.java.io :as io]
            [clojure.string  :as str]))

(defn board-dimensions
  [coords]
  (reduce (fn [[w h] [x y]] [(max w x) (max h y)]) [0 0] coords))

(defn parse-input
  [lines]
  (let [parse-fold     (juxt (comp #(if (= "y" %) 1 0) first)
                             (comp #(Long/parseLong %) last))
        [folds coords] (->> lines
                            (remove str/blank?)
                            (reverse)
                            (split-with #(str/starts-with? % "fold")))
        coords         (->> coords
                            (map #(str/split % #","))
                            (map (partial map #(Long/parseLong %)))
                            (mapv vec))]
    {:dimensions (board-dimensions coords)
     :coords     (set coords)
     :folds      (->> folds
                      (map #(-> (str/split % #" ")
                                (last)
                                (str/split #"=")
                                parse-fold))
                      (reverse)
                      vec)}))

(defn input
  [path]
  (->> path
       io/resource
       io/reader
       (line-seq)
       (parse-input)))

(defn print-board
  [{:keys [dimensions coords]}]
  (println)
  (println)
  (doseq [y (range (last dimensions))]
    (doseq [x (range (first dimensions))]
      (print (if (contains? coords [x y]) "#" ".")))
    (println)))

(defn fold-coord
  [[axis n] pos]
  (let [v (nth pos axis)]
    (if (< n v)
      (assoc pos axis (- n (- v n)))
      pos)))

(defn fold
  [{:keys [coords dimensions]} [axis n :as fold]]
  {:dimensions (assoc dimensions axis n)
   :coords     (into #{} (map (partial fold-coord fold) coords))})

(defn part1
  [path]
  (let [{:keys [folds] :as board} (input path)]
    (-> board
        (fold (first folds))
        :coords
        count)))

(defn process-folds
  [board]
  (reduce #(fold %1 %2) board (:folds board)))

(defn part2
  [path]
  (print-board
   (process-folds (input path)))
  0)

(comment
  (part1 "input13.txt")
  (part2 "input13.txt")
  )
