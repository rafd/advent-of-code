(ns aoc.year2018.day1
 (:require
   [clojure.string :as string]
   [aoc.helpers :as helpers]))

;; part 1

(defn part1 [input]
  (->> (string/split input #"\n")
       (map #(Integer. %))
       (apply +)))

#_(part1 (helpers/get-input 2018 1))

;; part 2

(defn part2 [input]
  (->> (string/split input #"\n")
       (map #(Integer. %))
       cycle
       (reductions +)
       (reduce (fn [seen x]
                 (if (contains? seen x)
                   (reduced x)
                   (conj seen x)))
               #{})))

#_(part2 "7\n7\n-2\n-7\n-4")
#_(part2 (helpers/get-input 2018 1))
