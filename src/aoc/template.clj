(ns aoc.template
  (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [hyperfiddle.rcf :as r]
   [aoc.helpers :as h]))

(defn part1 [input]
  (->> input
       string/split-lines
       (map (fn [line]
              line))))

(r/tests
 (part1 (h/get-input 2024 "3example")) := nil
 #_(part1 (h/get-input 2024 3)) := nil)

#_(defn part2 [input])

#_(r/tests
   (part2 (h/get-input 2024 "3example")) := nil
   #_(part2 (h/get-input 2024 3)) := nil)

