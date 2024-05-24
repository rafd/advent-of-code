(ns aoc.year2019.day1
 (:require
   [clojure.string :as string]
   [aoc.helpers :as helpers]))

(defn calculate-fuel [number]
  (- (quot number 3) 2))

(defn part1 [input]
  (->> input
       string/split-lines
       (map parse-long)
       (map calculate-fuel)
       (apply +)))

#_(part1 (helpers/get-input 2019 1))

(defn calculate-fuel-recursive [number]
  (let [fuel (calculate-fuel number)]
    (if (< 0 fuel)
      (+ fuel (calculate-fuel-recursive fuel))
      0)))

#_(calculate-fuel-recursive 100756)

(defn part2 [input]
  (->> input
       string/split-lines
       (map parse-long)
       (map calculate-fuel-recursive)
       (apply +)))

#_(part2 (helpers/get-input 2019 1))
