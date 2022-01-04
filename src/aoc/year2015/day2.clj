(ns aoc.year2015.day2
 (:require
   [clojure.string :as string]
   [aoc.helpers :as helpers]))

(defn part1 [input]
  (->> (string/split input #"\n")
       (map (fn [line]
             (sort (map #(Integer. %) (string/split line #"x")))))
       (map (fn [[a b c]]
             (+ (* 3 a b) (* 2 b c) (* 2 a c))))
       (apply +)))

#_(part1 (helpers/get-input 2015 2))

(defn part2 [input]
  (->> (string/split input #"\n")
       (map (fn [line]
             (sort (map #(Integer. %) (string/split line #"x")))))
       (map (fn [[a b c]]
             (+ (* 2 (+ a b))
                (* a b c))))
       (apply +)))

#_(part2 (helpers/get-input 2015 2))
