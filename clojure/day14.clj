(ns day14
  (:require [clojure.java.io :as io]))

(defn- parse-rule
  [s]
  [(take 2 s) (last s)])

(defn- input
  [path]
  (let [[formula _ & rules] (->> path io/resource io/reader line-seq)]
    [(seq formula) (into {} (map (comp parse-rule seq)) rules)]))

(defn- match-rule
  [rules pair]
  (let [x (get rules pair)]
    (take (if (some? x) 2 1) (interpose x pair))))

(defn- step
  [rules formula]
  (let [pairs (partition-all 2 1 formula)]
    (mapcat (partial match-rule rules) pairs)))

(defn- deduce-score
  [formula]
  (->> formula
       (frequencies)
       (sort-by val)
       (map val)
       ((juxt last first))
       (apply -)))

(defn- part1
  [path]
  (let [[template rules] (input path)]
    (->> (iterate (partial step rules) template)
         (drop 10)
         (first)
         (deduce-score))))

(def ^:private safe-plus
  (fnil + 0))

(defn- input2
  [path]
  (update (input path) 0 (comp (partial reduce #(update %1 %2 safe-plus 1) {})
                               (partial partition-all 2 1))))

(defn- deduce-score2
  [freqs]
  (->> freqs
       (reduce-kv #(update %1 (first %2) safe-plus %3) {})
       (sort-by val)
       (map val)
       ((juxt last first))
       (apply -)))

(defn- expand-pair-from-rules
  [rules m [l r :as k] v]
  (let [x (get rules k)]
    (cond-> (update m (into [] (remove nil?) [l x]) safe-plus v)
      (some? r)
      (update (into [] (remove nil?) [x r]) safe-plus v))))

(defn- expand-from-rules
  [rules freqs]
  (reduce-kv (partial expand-pair-from-rules rules) {} freqs))

(defn- part2
  [path]
  (let [[freqs rules] (input2 path)]
    (-> (iterate (partial expand-from-rules rules) freqs)
        (nth 40)
        (deduce-score2))))

(comment

  (part1 "input14.txt")
  (part2 "input14.txt")

)
