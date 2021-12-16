(ns aoc.year2021.day15
 (:require
   [aoc.helpers :refer [parse-input]]))

(def direction-vectors [[0 1] [0 -1] [1 0] [-1 0]])

(defn get-neighbors [[x y] grid]
  (->> direction-vectors
       (map (fn [[dx dy]]
              [(+ x dx) (+ y dy)]))
       (filter (fn [[x y]]
                 (and (<= 0 x (dec (count grid)))
                      (<= 0 y (dec (count (first grid)))))))))

#_(get-neighbors [0 0] [[1 7 4]
                        [1 2 2]
                        [1 1 1]])

(defn initial-cost-grid [start grid]
  (-> (mapv #(mapv (fn [_] ##Inf) %) grid)
      (assoc-in start 0)))

#_(initial-cost-grid [0 0] [[1 2] [3 4]])

(defn travel-costs-recur-unsafe [current-point cost-grid grid]
 (let [neighbors (get-neighbors current-point grid)
       new-cost-grid (->> neighbors
                          (reduce
                           (fn [memo neighbor]
                             (update-in memo neighbor
                               (fn [old-cost]
                                 (min old-cost
                                      (+ (get-in cost-grid current-point)
                                         (get-in grid neighbor))))))
                           cost-grid))]
   (->> neighbors
        (reduce (fn [memo neighbor]
                  (if (< (get-in memo current-point)
                         (get-in memo neighbor))
                    (travel-costs-recur-unsafe neighbor memo grid)
                    memo))
                new-cost-grid))))

(defn travel-costs-unsafe [start grid]
  (travel-costs-recur-unsafe start (initial-cost-grid start grid) grid))

;; safe

(defn travel-costs-recur [points cost-grid delta-grid]
  (if (empty? points)
    cost-grid
    (let [current-point (first points)
          neighbors (get-neighbors current-point delta-grid)
          points (rest points)
          new-destinations (->> neighbors
                                (filter (fn [neighbor]
                                          (< (+ (get-in cost-grid current-point)
                                                (get-in delta-grid neighbor))
                                             (get-in cost-grid neighbor)))))
          new-cost-grid (->> new-destinations
                             (reduce
                              (fn [memo neighbor]
                                (assoc-in memo neighbor
                                  (+ (get-in cost-grid current-point)
                                     (get-in delta-grid neighbor))))
                              cost-grid))
          points (->> (into points new-destinations)
                      (sort-by (fn [point] (get-in new-cost-grid point))))]

      (recur points new-cost-grid delta-grid))))

(defn travel-costs [start grid]
  (travel-costs-recur [start] (initial-cost-grid start grid) grid))


#_(travel-costs [0 0] [[1 7 4]
                       [1 2 2]
                       [1 1 1]])
;; expect:
#_[[0 7 9]
   [1 3 5]
   [2 3 4]]

(defn shortest-path [start end grid]
  (get-in (travel-costs start grid) end))

;; part 1

#_(let [grid (->> (parse-input 2021 "15" "\n")
                  (mapv (fn [row] (mapv #(Integer. (str %)) row))))]
    (shortest-path [0 0] [(dec (count grid))
                          (dec (count (first grid)))] grid))

;; part 2

(defn manipulate-grid [grid]
  (let [[xmax ymax] [(count grid) (count (first grid))]]
   (vec (for [x (range 0 (* 5 xmax))]
          (vec (for [y (range 0 (* 5 ymax))]
                 (let [v (+ (quot x xmax)
                          (quot y ymax)
                          (get-in grid [(mod x xmax)
                                        (mod y ymax)]
                            10))]
                  (if (<= 10 v)
                    (- v 9)
                    v))))))))


#_(manipulate-grid [[8]])

#_(time (let [grid (->> (parse-input 2021 "15" "\n")
                        (mapv (fn [row] (mapv #(Integer. (str %)) row)))
                        manipulate-grid)]
          #_(spit "grid.txt" grid)
          (shortest-path [0 0] [(dec (count grid))
                                (dec (count (first grid)))] grid)))
;; 153606ms
