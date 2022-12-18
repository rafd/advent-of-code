(ns aoc.year2022.day12
  (:require
   [aoc.helpers :as h]
   [hyperfiddle.rcf :refer [tests]]
   [aoc.dijkstra :as dijkstra]))

(defn indexes-of-2d [value grid]
  (filter identity (for [x (range 0 (count grid))
                         y (range 0 (count (first grid)))]
                     (when (= value (get-in grid [x y]))
                       [x y]))))

(defn char->height [char]
  (case char
    \S (char->height \a)
    \E (char->height \z)
    (- (int char) (int \a))))

(defn parse [input]
  (let [string-grid (->> input
                         (h/rsplit "\n")
                         (mapv vec))
        start-location (first (indexes-of-2d \S string-grid))
        end-location  (first (indexes-of-2d \E string-grid))
        height-grid (->> string-grid
                         (mapv (fn [line]
                                 (mapv char->height line))))]
    {:grid height-grid
     :start start-location
     :end end-location}))

(defn part1 [input]
  (let [{:keys [grid start end]} (parse input)]
    (dijkstra/shortest-path
     (fn [current-value neighbor-value]
       (if (<= neighbor-value (inc current-value))
         1
         ##Inf))
     start
     end
     grid)))

(tests
 (part1 (h/get-input 2022 "12example")) := 31)

#_(part1 (h/get-input 2022 12))

(defn part2 [input]
  (let [{:keys [grid end]} (parse input)
        potential-starts (indexes-of-2d 0 grid)]
   (->> potential-starts
         (map (fn [start]
                (dijkstra/shortest-path
                 (fn [current-value neighbor-value]
                   (if (<= neighbor-value (inc current-value))
                     1
                     ##Inf))
                 start
                 end
                 grid)))
         (apply min))))

(tests
 (part2 (h/get-input 2022 "12example")) := 29)

#_(part2 (h/get-input 2022 12))
