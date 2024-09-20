(ns aoc.year2019.day3
  (:require
    [clojure.set :as set]
    [clojure.string :as string]
    [hyperfiddle.rcf :as rcf]
    [aoc.helpers :as helpers]))

(defn manhattan-distance-from-origin
  [[x y]]
  (+ (abs x) (abs y)))

(rcf/tests
  (manhattan-distance-from-origin [3 -4]) := 7)

(def vector+
  (partial mapv +))

(rcf/tests
  (vector+ [1 2] [3 4]) := [4 6])

(def offset
  {:L [-1  0]
   :R [ 1  0]
   :U [ 0  1]
   :D [ 0 -1]})

(defn part-1
  [input]
  (->> input
       (string/split-lines)
       (map (fn [path]
              (->> (string/split path #",")
                   (map (fn [s]
                          (let [[_ direction amount] (re-matches #"([RDUL])(\d{1,})" s)]
                            {:direction (keyword direction)
                             :amount (parse-long amount)}))))))
       ;; [[{:direction :R, :amount 8} ...] ...]
       ;; for each wire, convert it to the points it passes
       (map (fn [path]
              (->> path
                   (mapcat (fn [{:keys [direction amount]}]
                             (repeat amount (offset direction))))
                   (reductions vector+))))
       ;; [[[1 0] [2 0] ...] ...]
       (map set)
       ;; "find the intersection point closest to the central port" (0, 0)
       (apply set/intersection)
       (map manhattan-distance-from-origin)
       (apply min)))

(rcf/tests
  (part-1 "R8,U5,L5,D3\nU7,R6,D4,L4") := 6
  (part-1 "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83") := 159
  (part-1 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7") := 135
  (part-1 (helpers/get-input 2019 3)) := 2427)

