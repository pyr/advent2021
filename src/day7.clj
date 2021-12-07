(ns day7
  (:require [clojure.java.io :as io]
            [clojure.string  :as str]))

(defn- input
  [path]
  (-> (io/resource path)
      (slurp)
      (str/trim)
      (str/split #",")
      (->> (map #(Long/parseLong %)))))

(defn- raw-distance
  [x y]
  (- (max x y) (min x y)))

(defn- single-budget
  [distance-fn domain start-position]
  (reduce #(assoc %1 %2 (distance-fn start-position %2)) {} domain))

(defn- budgets
  [distance-fn domain positions]
  (into [] (map (partial single-budget distance-fn domain)) positions))

(defn- combine-budget
  [m budget]
  (reduce-kv #(update %1 %2 (fnil + 0) %3) m budget))

(defn- combine-budgets
  [budgets]
  (reduce combine-budget {} budgets))

(defn- domain-for
  [positions]
  (range (reduce min positions) (inc (reduce max 0 positions))))

(defn part1
  [path]
  (let [positions (input path)]
    (->> positions
         (budgets raw-distance (domain-for positions))
         (combine-budgets)
         (map val)
         (reduce min))))

;; part2
(defn- growing-distance
  [acc x]
  (if (zero? x)
    acc
    (recur (+ acc x) (dec x))))

(defn- growing-distances
  [raw-distances]
  (reduce #(assoc %1 %2 (growing-distance 0 %2)) {} raw-distances))

(defn part2
  [path]
  ;; Computes all possible distances between start-positions and possible
  ;; positions in the domain (natural ints between minimum and maximum position)
  ;; Then runs budgets, combines them and picks lowest value as for part1
  (let [positions (input path)
        domain    (domain-for positions)
        combined  (into #{} (for [x positions y domain] (raw-distance x y)))
        distances (growing-distances combined)]
    (->> positions
         (budgets (fn [x y] (get distances (raw-distance x y))) domain)
         (combine-budgets)
         (map val)
         (reduce min))))

(comment
  (part1 "input7.txt")
  (part2 "input7.txt")
)
