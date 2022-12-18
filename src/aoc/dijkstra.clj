(ns aoc.dijkstra
  (:require
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

(defn travel-costs-recur [cost-fn points cost-grid base-grid]
  (if (empty? points)
    cost-grid
    (let [[current-point current-point-cost] (peek points)
          neighbors (get-neighbors current-point base-grid)
          new-destinations (->> neighbors
                                (map (fn [neighbor]
                                       (let [new-cost (+ current-point-cost
                                                         (cost-fn (get-in base-grid current-point)
                                                                  (get-in base-grid neighbor)))
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
      (recur cost-fn points new-cost-grid base-grid))))

(defn travel-costs [cost-fn start grid]
  (travel-costs-recur cost-fn
                      (priority-map start 0)
                      (initial-cost-grid start grid)
                      grid))

#_(travel-costs
   (fn [current-value neighbor-value] neighbor-value)
   [0 0]
   [[1 7 4]
    [1 2 2]
    [1 1 1]])

(defn shortest-path [cost-fn start end grid]
  (get-in (travel-costs cost-fn start grid) end))
