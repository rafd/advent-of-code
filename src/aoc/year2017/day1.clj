(ns aoc.year2017.day1
 (:require
   [aoc.helpers :refer [parse-input]]))

;; part 1

#_(->> (parse-input 2017 1 "")
       (map parse-long)
       ((fn [x] (partition 2 1 x x)))
       (filter (fn [[x y]] (= x y)))
       (map first)
       (reduce +))

;; part 2

#_(let [numbers (->> (parse-input 2017 1 "")
                     (map parse-long))]
   (->> (split-at (/ (count numbers) 2) numbers)
        (apply map vector)
        (filter (fn [[x y]] (= x y)))
        (map first)
        (reduce +)
        (* 2)))
