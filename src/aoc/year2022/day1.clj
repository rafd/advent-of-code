(ns aoc.year2022.day1
  (:require
    [clojure.string :as string]
    [com.rpl.specter :as x]
    [hyperfiddle.rcf :refer [tests]]
    [aoc.helpers :as h]))

(defn parse [input-string]
  (-> input-string
      (string/split #"\n\n")
      (->> (map (fn [x]
                  (reduce + (map (fn [y] (Integer. y))
                                 (string/split x #"\n"))))))))
(defn day1part1
  [input-string]
  (->> (parse input-string)
       (apply max)))

(defn day1part2
  [input-string]
  (->> (parse input-string)
       (sort)
       (take-last 3)
       (reduce +)))

(tests
 (day1part1 (h/get-input 2022 "1example")) := 24000
 (day1part2 (h/get-input 2022 "1example")) := 45000)

#_(day1part1 (h/get-input 2022 1))
#_(day1part2 (h/get-input 2022 1))

(defn day1part1spectre [input-string]
  (->> input-string
       (x/transform [] #(string/split % #"\n\n"))
       (x/transform [x/ALL] #(string/split % #"\n"))
       (x/transform [x/ALL x/ALL] #(Integer. %))
       (x/transform [x/ALL] #(reduce + %))
       (apply max)))

(defn day1part2spectre [input-string]
  (->> input-string
       (x/transform [] #(string/split % #"\n\n"))
       (x/transform [x/ALL] #(string/split % #"\n"))
       (x/transform [x/ALL x/ALL] #(Integer. %))
       (x/transform [x/ALL] #(reduce + %))
       (sort)
       (take-last 3)
       (reduce +)))

(tests
  (day1part1spectre (h/get-input 2022 "1example")) := 24000
  (day1part2spectre (h/get-input 2022 "1example")) := 45000)
