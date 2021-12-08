(ns day8
  (:require [clojure.java.io :as io]
            [clojure.string  :as str]))

(defn input
  [path]
  (let [subsplit (fn [line]
                   (let [[signals outval] (str/split line #"\|")]
                     [(str/split (str/trim signals) #" ")
                      (str/split (str/trim outval) #" ")]))]
    (->> path
         io/resource
         io/reader
         line-seq
         (map subsplit))))

(defn part1
  [path]
  (->> (input path)
       (map last)
       (mapcat (partial map count))
       (filter #{2 3 4 7})
       (count)))

(comment
  (take 2 (input "input8.txt"))
  (take 2 (part1 "input8.txt"))
  )
