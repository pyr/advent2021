(ns runner)

(defn run
  [_]
  (doseq [i (map inc (range 10))]
    (require (symbol (str "day" i))))
  (doseq [i (map inc (range 10))
          :let [part1 (ns-resolve (symbol (str "day" i)) 'part1)
                part2 (ns-resolve (symbol (str "day" i)) 'part2)
                input (str "input" i ".txt")]]
    (printf "day%02d_part1: %s\nday%02d_part2: %s\n" i (part1 input) i (part2 input))))
