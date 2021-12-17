(ns day17)

(def target-pos [269 292 -68 -44])

;; There's probably a deterministic way to do this
;; but right now I'm too tired

(defn matches-target
  [init-velx init-vely [minx maxx miny maxy]]
  (loop [step  0
         x     0
         y     0
         velx  init-velx
         vely  init-vely
         highy 0]
    (cond
      (and (<= minx x maxx)
           (<= miny y maxy))
      [init-velx init-vely highy]

      (and (neg? vely) (< y miny))
      nil

      (> step 500)
      nil

      :else
      (recur (inc step)
             (+ x velx)
             (+ y vely)
             (cond
               (pos? velx) (dec velx)
               (neg? velx) (inc velx)
               :else       velx)
             (dec vely)
             (if (pos? vely) (+ y vely) highy)))))

(defn find-target
  [pos]
  (->> (for [velx (range 100)
             vely (range 100)
             :let [match (matches-target velx vely pos)]
             :when (some? match)]
         match)
       (reduce #(if (> (last %2) (last %1)) %2 %1) [0 0 0])
       (last)))

(defn part1
  [& _]
  (find-target target-pos))

(defn count-matches
  [pos]
  (->> (for [velx (range 300)
             vely (range -300 300)
             :let [match (matches-target velx vely pos)]
             :when (some? match)]
         match)
       (count)))

(defn part2
  [& _]
  (count-matches target-pos))

(comment
  (part1)
  (part2)
)
