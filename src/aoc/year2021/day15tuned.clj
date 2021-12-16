(ns aoc.year2021.day15tuned
 (:require
   [aoc.helpers :refer [parse-input]]
   [clojure.data.priority-map :refer [priority-map]]))

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

(defn travel-costs-recur [points cost-grid delta-grid]
  (if (empty? points)
    cost-grid
    (let [[current-point current-point-cost] (peek points)
          neighbors (get-neighbors current-point delta-grid)
          new-destinations (->> neighbors
                                (map (fn [neighbor]
                                       (let [new-cost (+ current-point-cost
                                                         (get-in delta-grid neighbor))
                                             keep? (< new-cost
                                                      (get-in cost-grid neighbor))]
                                        [neighbor new-cost keep?])))
                                (filter (fn [[_ _ keep?]] keep?)))
          new-cost-grid (->> new-destinations
                             (reduce
                              (fn [memo [neighbor new-cost]]
                                (assoc-in memo neighbor new-cost))
                              cost-grid))
          points (into (pop points) new-destinations)]
      (recur points new-cost-grid delta-grid))))

(defn travel-costs [start grid]
  (travel-costs-recur (priority-map start 0) (initial-cost-grid start grid) grid))

#_(travel-costs [0 0] [[1 7 4]
                       [1 2 2]
                       [1 1 1]])
;; expect:
#_[[0 7 9]
   [1 3 5]
   [2 3 4]]

(defn shortest-path [start end grid]
  (get-in (travel-costs start grid) end))

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
                        manipulate-grid)
              start [0 0]
              end [(dec (count grid))
                   (dec (count (first grid)))]]
          (shortest-path start end grid)))
;; 153606ms
;; use pqueue
;; 7532ms
