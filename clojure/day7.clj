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

(defn- small-cost
  [x y]
  (Math/abs (- x y)))

(defn- large-cost
  [x y]
  (let [n (small-cost x y)]
    (long (/ (* n (dec n)) 2))))

(defn- domain-for
  [positions]
  (range (reduce min positions) (inc (reduce max 0 positions))))

(defn compute-costs
  [cost-fn positions destination]
  (reduce-kv #(+ %1 (* (cost-fn %2 destination) %3)) 0 positions))

(defn best-cost
  [cost-fn path]
  (let [positions (input path)
        domain    (domain-for positions)]
    (->> (map (partial compute-costs cost-fn (frequencies positions)) domain)
         (reduce min))))

;; part2
(def part1 (partial best-cost small-cost))
(def part2 (partial best-cost large-cost))

(comment
  (part1 "input7.txt")
  (part2 "input7.txt")
)
