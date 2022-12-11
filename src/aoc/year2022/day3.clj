(ns aoc.year2022.day3
  (:require
   [clojure.set :as set]
   [aoc.helpers :as h]
   [hyperfiddle.rcf :refer [tests]]))

(defn priority [letter]
  (if (re-matches #"[A-Z]" (str letter))
    (- (int letter) (- (int \A) 27))
    (- (int letter) (- (int \a) 1))))

(tests
 (priority \a) := 1
 (priority \z) := 26
 (priority \A) := 27
 (priority \Z) := 52)

(defn part1 [input]
  (->> input
       (map (fn [line]
               (->> (split-at (/ (count line) 2) line)
                    (map set)
                    (apply set/intersection)
                    first
                    priority)))
       (reduce +)))

(tests
 (part1 (h/parse-input 2022 "3example" "\n")) := 157)

#_(part1 (h/parse-input 2022 3 "\n"))

(defn part2 [input]
  (->> input
       (partition 3)
       (map (fn [group]
              (->> group
                   (map set)
                   (apply set/intersection)
                   first
                   priority)))
       (reduce +)))

(tests
 (part2 (h/parse-input 2022 "3example" "\n")) := 70)

#_(part2 (h/parse-input 2022 3 "\n"))
