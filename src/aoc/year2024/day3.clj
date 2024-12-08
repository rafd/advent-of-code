(ns aoc.year2024.day3
  (:require
   [clojure.string :as string]
   [hyperfiddle.rcf :as r]
   [aoc.helpers :as h]))

(defn part1 [input]
  (->> (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" input)
       ;; (["mul(2,4)" "2" "4"] ... )
       (map rest)
       (map #(map parse-long %))
       (map (partial apply *))
       (apply +)))

(r/tests
 (part1 (h/get-input 2024 "3example")) := 161
 (part1 (h/get-input 2024 3)) := 171183089)

(defn part2 [input]
  (->> (str "do()" (string/replace input "\n" "") "don't()")
       (re-seq #"do\(\)(.+?)don't\(\)")
       (map second)
       (map part1)
       (apply +)))

(r/tests
 (part2 (h/get-input 2024 "3example2")) := 48
 (part2 (h/get-input 2024 3)) := 63866497)
