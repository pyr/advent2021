(ns day6
  (:require  [clojure.java.io :as io]
             [clojure.string  :as str]))

(defn- input
  [path]
  (-> (io/resource path)
      (slurp)
      (str/trim)
      (str/split #",")
      (->> (mapv #(Long/parseLong %)))))

(defn- tick!
  [m k v]
  (cond-> m
    (zero? k) (-> (update 6 (fnil + 0) v) (update 8 (fnil + 0) v))
    (pos? k)  (-> (update (dec k) (fnil + 0) v))))

(defn- count-fishes
  [days path]
  (loop [day    0
         fishes (frequencies (input path))]
    (if (< day days)
      (recur (inc day) (reduce-kv tick! {} fishes))
      (reduce + (vals fishes)))))

(def part1 (partial count-fishes 80))
(def part2 (partial count-fishes 256))

(comment
  (part1 "input6.txt")
  (part2 "input6.txt")
  )
