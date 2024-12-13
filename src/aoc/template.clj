(ns aoc.template
  (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [hyperfiddle.rcf :as r]
   [aoc.helpers :as h]))

(defn parse-line [line]
  (->> line))

(defn part1 [input]
  (->> input
       (map parse-line)))

(r/tests
 (part1 (h/get-input 2024 "3example")) := nil)

#_(part1 (h/get-input 2024 3))

(defn part2 [input]
  (->> input
       (map parse-line)))

#_(r/tests
   (part2 (h/get-input 2024 "3example")) := nil)

#_(part2 (h/get-input 2024 3))
