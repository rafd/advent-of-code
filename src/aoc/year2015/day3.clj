(ns aoc.year2015.day3
 (:require
   [clojure.string :as string]
   [aoc.helpers :as helpers]))

(defn move [[x y] dir]
  (case dir
    \^ [x (inc y)]
    \> [(inc x) y]
    \v [x (dec y)]
    \< [(dec x) y]))

(defn part1 [input]
  (->> (reductions move [0 0] input)
       distinct
       count))

#_(part1 (helpers/get-input 2015 3))

(defn part2 [input]
  )

#_(part2 (helpers/get-input 2015 3))
