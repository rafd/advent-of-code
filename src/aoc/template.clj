(ns aoc.template
  (:require
   [hyperfiddle.rcf :as r]
   [aoc.helpers :as h]))

(defn parse-line [line]
  line)

(defn part1 [input]
  (->> input
       (map parse-line)))

(r/tests
 (part1 (h/parse-input 2023 "1example" "\n")) := nil)

#_(part1 (h/parse-input 2023 1 "\n"))

(defn part2 [input]
  (->> input
       (map parse-line)))

(r/tests
 (part2 (h/parse-input 2023 "1example" "\n")) := nil)

#_(part2 (h/parse-input 2023 1 "\n"))
