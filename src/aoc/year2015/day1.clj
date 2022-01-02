(ns aoc.year2015.day1
 (:require
   [aoc.helpers :refer [parse-input]]))

;; part 1

(->> (parse-input 2015 1 "")
     (map {"(" 1 ")" -1})
     (reduce +))

;; part 2

(->> (parse-input 2015 1 "")
     (map {"(" 1 ")" -1})
     (reductions +)
     ((fn [x] (.indexOf x -1)))
     ;; b/c they want to start at 1
     inc)
