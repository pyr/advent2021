(ns day8
  (:require [clojure.java.io :as io]
            [clojure.set     :as set]
            [clojure.string  :as str]))

(defn input
  [path]
  (let [subsplit (fn [line]
                   (mapv (fn [x] (into #{} (seq x)))
                         (-> (str/replace line "| " "")
                             (str/trim)
                             (str/split #" "))))]
    (->> path
         io/resource
         io/reader
         line-seq
         (map subsplit))))

(defn part1
  [path]
  (->> (input path)
       (mapcat #(drop 10 %))
       (map count)
       (filter #{2 3 4 7})
       (count)))

;; Not super happy with part2, I feel like I'm missing an elegant solution
;;
;; Sets are built for every input pattern and the value to resolve.
;; To translate the patterns into their corresponding digit value the naive
;; algorithm is used:
;; -1. we know upfront who 1, 4, 7, and 8 are
;; -2. of the candidates having six segments lit up, 9 is the one which
;;     has all the segments present for the digit 4. store it and drop from
;;     candidates
;; -3. of the remaining candidates having six segements, 0 is the one which
;;     has all the segments present for the digit 1. store it and drop from
;;     candidates
;; -4. store the remaining candidate having six segments as 6
;; -5. of the candidates having five segments lit up, 3 is the one which
;;     has all the segments present for the digit 1. store it and drop from
;;     candidates
;; -6. of the candidates having five segments lit up, 5 is the one which
;;     has all the segments present for the digit 4. store it and drop from
;;     candidates
;; -7. store the last one as 2 and return

(defn- resolve-pattern
  [{:keys [xs] :as acc} [digit destination]]
  (let [[pattern & tail] (sort-by #(count (set/difference digit %)) xs)]
    (-> acc
        (assoc-in [:m destination] pattern)
        (assoc :xs tail))))

(defn- resolve-patterns
  [[p0 p1 p2 p3 p4 p5 p6 p7 p8 p9]]
  ;; map keys are the digit, values corresponding pattern
  (let [inits   {1 p0 4 p2 7 p1 8 p9}
        ;; lookups are pairs of where to look up (candidates) and
        ;; a collection of pairs of who to test against and digit
        ;; to store the winning result at
        lookups [[p6 p7 p8] [[p2 9] [p0 0] [p0 6]]
                 [p3 p4 p5] [[p0 3] [p2 5] [p2 2]]]]
    (->> (for [[xs ts] (partition 2 lookups)]
           (:m (reduce resolve-pattern {:m {} :xs xs} ts)))
         (reduce merge inits) ;;
         (set/map-invert))))  ;; invert the map

(defn build-number
  [xs]
  (let [patterns (->> xs (take 10) (sort-by count) resolve-patterns)
        digits   (->> xs (drop 10) reverse)]
    (->> (for [[d i] (partition 2 (interleave digits (range)))]
           (* (long (Math/pow 10 i)) (get patterns d)))
         (reduce +))))

(defn part2
  [path]
  (->> (input path)
       (map build-number)
       (reduce +)))

(comment

  (part1 "input8.txt")
  (part2 "input8.txt")


  )
