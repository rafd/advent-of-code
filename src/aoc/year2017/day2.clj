(ns aoc.year2017.day2
 (:require
   [clojure.string :as string]
   [clojure.math.combinatorics :as combo]
   [aoc.helpers :as helpers]))

(defn part1 [input]
  (->> input
       string/split-lines
       (map (fn [line]
             (map parse-long (string/split line #"\t"))))
       (map (fn [line]
             (- (apply max line)
                (apply min line))))
       (apply +)))

#_(part1 (helpers/get-input 2017 2))

(defn part2 [input]
  (->> input
       string/split-lines
       (map (fn [line]
             (map parse-long (string/split line #"\t"))))
       (map (fn [line]
             (->> (combo/combinations line 2)
                  (map sort)
                  (map reverse)
                  (filter (fn [[a b]]
                           (= 0 (rem a b))))
                  first
                  (apply quot))))
       (apply +)))

#_(part2 (helpers/get-input 2017 2))
