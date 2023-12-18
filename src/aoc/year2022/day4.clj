(ns aoc.year2022.day4
  (:require
   [aoc.helpers :as h]
   [hyperfiddle.rcf :refer [tests]]))

(defn fully-contained? [a b c d]
  (or (and (<= c a) (<= b d))
      (and (<= a c) (<= d b))))

(defn part1 [input]
  (->> input
       (map (fn [line]
              (->> (re-matches #"(\d+)-(\d+),(\d+)-(\d+)" line)
                   rest
                   (map parse-long)
                   (apply fully-contained?))))
       (filter true?)
       count))

(tests
 (part1 (h/parse-input 2022 "4example" "\n")) := 2)

#_(part1 (h/parse-input 2022 4 "\n"))

(defn overlap? [start1 end1 start2 end2]
  (or (<= start1 start2 end1)
      (<= start1 end2 end1)
      (<= start2 start1 end2)
      (<= start2 end1 end2))) ;; last case redundant

(defn part2 [input]
  (->> input
       (map (fn [line]
              (->> (re-matches #"(\d+)-(\d+),(\d+)-(\d+)" line)
                   rest
                   (map parse-long)
                   (apply overlap?))))
       (filter true?)
       count))

(tests
 (part2 (h/parse-input 2022 "4example" "\n")) := 4)

#_(part2 (h/parse-input 2022 4 "\n"))


;;    |-------|
;;      |-------|

;;      |-------|
;;    |-------|

;;      |---|
;;    |-------|

;;    |-------|
;;      |---|
