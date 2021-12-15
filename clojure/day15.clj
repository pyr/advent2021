(ns day15
  (:require [clojure.java.io :as io]))

(defn- parse-row
  [[y row]]
  (for [[x c] (map-indexed vector row)] [[x y] (- (int c) (int \0))]))

(defn input
  [path]
  (let [lines (-> path io/resource io/reader line-seq)
        size  (count (first lines))]
    {:size   size
     :target [(dec size) (dec size)]
     :grid   (into {} (comp (map-indexed vector) (mapcat parse-row)) lines)}))

(defn adjacents-fn
  [{:keys [size grid]}]
  (fn [[x y]]
    (let [invalid? #(or (= % [0 0]) (some neg? %) (some (partial <= size) %))]
      (eduction
       (comp
        (remove invalid?)
        (map (juxt identity (partial get grid))))
       [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]))))

(defn dijkstra
  [adjacents]
  (let [minpair #(first (sort-by val %))]
    (loop [q    {[0 0] 0}
           seen {}]
      ;; This is the slow part, since we use a
      (if-let [[pos cost] (minpair q)]
        ;; The following is a bit more contrived than what we'd get with a simple
        ;; ->> but it tries to speed things up. I'm trying to keep with plain
        ;; Clojure and no additional dependencies, otherwise c.d.priority-map would
        ;; likely give a good speed boost here
        (recur (reduce #(update %1 (first %2) (fnil min Long/MAX_VALUE)
                                (+ cost (last %2)))
                       (dissoc q pos)
                       (eduction (remove #(contains? seen (first %))) (adjacents pos)))
               (assoc seen pos cost))
        seen))))

(defn fast-dijkstra
  [adjacents]
  ;; Ugly but way faster version of dijkstra. The main problem with the
  ;; version above is the constant sorting that gets done on the same
  ;; dataset.
  ;;
  ;; What this version does instead is hold on to two values: a map and
  ;; a sorted set. The map allows fast lookups by position, and the sorted
  ;; set allows popping the lowest available cost path quickly.
  ;;
  ;; As hinted to above, this is more or less what c.d.priority-map does.
  ;;
  ;; This version drops down to ~ 5s from the 50s for the large input
  (loop [{:keys [qs] :as q} {:qm {[0 0] 0} :qs (sorted-set [0 [0 0]])}
         seen               {}]
    (if-let [[cost pos] (first qs)]
      (recur (reduce (fn [{:keys [qm] :as q} [k add-cost]]
                       (if-let [prev (get qm k)]
                         (let [v       (min prev (+ cost add-cost))
                               update? (= prev v)]
                           (cond-> (assoc-in q [:qm k] v)
                             update? (-> (update :qs disj [prev k])
                                         (update :qs conj [v k]))))
                         (-> q
                             (assoc-in [:qm k] (+ cost add-cost))
                             (update :qs conj [(+ cost add-cost) k]))))
                     (-> q
                         (update :qm dissoc pos)
                         (update :qs disj [cost pos]))
                     (eduction (remove #(contains? seen (first %))) (adjacents pos)))
             (assoc seen pos cost))
      seen)))

(defn part1
  [path]
  (let [board (input path)]
    (get (fast-dijkstra (adjacents-fn board)) (:target board))))

(defn expand-cells
  [grid size]
  (let [increments (map (partial * size) (range 5))]
    (for [x    increments
          y    increments
          xoff (range size)
          yoff (range size)]
      [[(+ x xoff) (+ y yoff)]
       (let [v (+ (get grid [xoff yoff]) (quot x size) (quot y size))]
         (cond-> v (> v 9) (- 9)))])))

(defn expand-board
  [{:keys [size] :as board}]
  (let [size (* size 5)]
    (-> board
        (assoc :size size)
        (assoc :target [(dec size) (dec size)])
        (assoc :grid (into {} (expand-cells (:grid board) (:size board)))))))

(defn part2
  [path]
  (let [board (expand-board (input path))]
    (get (fast-dijkstra (adjacents-fn board)) (:target board))))

(comment

  (time
   (part1 "input15.txt"))

  (time
   (part2 "input15.txt"))

  )
