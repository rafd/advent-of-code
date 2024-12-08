(ns aoc.year2024.day2
  (:require
   [clojure.string :as string]
   [hyperfiddle.rcf :as rcf]
   [aoc.helpers :as h]))

(rcf/enable!)

(defn deltas [coll]
  (map - (rest coll) coll))

(rcf/tests
  (deltas [1 2 3 4]) := [1 1 1])

(defn safe? [nums]
  (let [ds (deltas nums)]
    (and (or (every? pos? ds)
             (every? neg? ds))
         (every? (fn [x] (<= -3 x 3)) ds))))

(defn part1 [input]
  (->> input
       string/split-lines
       (map (fn [line] (map parse-long (string/split line #" "))))
       (filter safe?)
       count))

(rcf/tests
 (part1 (h/get-input 2024 "2sample")) := 2
 (part1 (h/get-input 2024 "2")) := 252)

(defn extract-at-index [coll index]
  (concat (take index coll) (drop (inc index) coll)))

(rcf/tests
 (extract-at-index [1 2 3 4] 1) := [1 3 4])

(defn safe-with-dampener? [nums]
  (->> (range 0 (count nums))
       (map (partial extract-at-index nums))
       (some safe?)
       boolean))

(defn part2 [input]
  (->> input
       string/split-lines
       (map (fn [line] (map parse-long (string/split line #" "))))
       (filter safe-with-dampener?)
       count))

(rcf/tests
 (part2 (h/get-input 2024 "2sample")) := 4
 (part2 (h/get-input 2024 2)) := 324)

