(ns day10
  (:require [clojure.java.io :as io]))

(defn input
  [path]
  (->> path
       io/resource
       io/reader
       line-seq))

(def corruption-cost    {\) 3 \] 57 \} 1197 \> 25137})
(def completion-cost    {\) 1 \] 2 \} 3 \> 4})
(def closing-equivalent {\[ \] \( \) \{ \} \< \>})
(def closing?           (partial contains? #{\) \] \} \>}))
(def corrupt?           (comp (partial = :corrupted) first))
(def incomplete?        (comp (partial = :incomplete) first))

(defn parse-line
  [line]
  (loop [[c & cs] (seq line)
         stack    nil]
    (cond
      (and (nil? c) (seq stack))
      [:incomplete (reduce str stack)]

      (nil? c)
      [:complete]

      (and (closing? c) (not= (first stack) c))
      [:corrupted c (corruption-cost c)]

      (closing? c)
      (recur cs (drop 1 stack))

      :else
      (recur cs (conj stack (closing-equivalent c))))))

(defn part1
  [path]
  (->> path
       input
       (map parse-line)
       (filter corrupt?)
       (frequencies)
       (reduce-kv #(+ %1 (* (last %2) %3)) 0)))

(defn total-completion-cost
  [stack]
  (reduce #(+ (completion-cost %2) (* 5 %1)) 0 stack))

(defn get-median
  [scores]
  (nth scores  (/ (count scores) 2)))

(defn part2
  [path]
  (->> path
       input
       (map parse-line)
       (filter incomplete?)
       (map (comp total-completion-cost last))
       (sort)
       (get-median)))

(comment
  (part1 "input10.txt")
  (part2 "input10.txt")
  )
