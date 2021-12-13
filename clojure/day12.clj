(ns day12
  (:require [clojure.java.io :as io]
            [clojure.string  :as str]))

(defn parse-line
  [s]
  (let [[x y] (mapv keyword (str/split s #"-"))]
    [[x y] [y x]]))

(defn input
  [path]
  (->> path
       io/resource
       io/reader
       (line-seq)
       (mapcat parse-line)
       (reduce #(update %1 (first %2) conj (last %2)) {})))

(defn small?
  [s]
  (= (name s) (str/lower-case (name s))))

(def large? (complement small?))

(defn viable?
  [prefix destinations]
  (remove (set prefix) destinations))

(defn viable-destinations
  [graph prefix]
  (let [visited    (into #{} (filter small?) prefix)
        candidates (set (get graph (last prefix)))]
    (remove (partial contains? visited) candidates)))

(defn viable-destinations-twice
  [graph prefix]
  (let [candidates    (remove (partial = :start) (get graph (last prefix)))
        small-visited (filter small? prefix)
        twice?        (not= (count small-visited)
                             (count (set small-visited)))]
    (if twice?
      (viable-destinations graph prefix)
      candidates)))

(defn find-paths
  [viable-fn graph prefix]
  (if (= :end (last prefix))
    [prefix]
    (mapcat #(find-paths viable-fn graph (conj prefix %))
            (viable-fn graph prefix))))

(defn path-count
  [viable-fn input]
  (count
   (find-paths viable-fn input [:start])))

(defn part1
  [path]
  (path-count viable-destinations (input path)))

(defn part2
  [path]
  (path-count viable-destinations-twice (input path)))

(comment
  (part1 "input12.txt")
  (part2 "input12.txt")
  )
