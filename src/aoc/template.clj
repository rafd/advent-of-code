(ns aoc.template
  (:require
   [clojure.string :as string]
   [hyperfiddle.rcf :as r]
   [aoc.helpers :as h]))

(defn finalize [input])

(defn part1 [input]
  (->> input
       finalize))

(r/tests
 (part1 (h/parse-input 2023 1 "\n")) := nil)

(defn part2 [input]
  (->> input
       finalize))

(r/tests
 (part2 (h/parse-input 2023 1 "\n")) := nil)
