(ns aoc.year2023.day1
  (:require
   [aoc.helpers :as h]
   [clojure.string :as string]
   [hyperfiddle.rcf :refer [tests]]))

(defn process-line [line]
  (let [numbers (->> line
                     (filter #{\1 \2 \3 \4 \5 \6 \7 \8 \9 \0})
                     (map (fn [i] (Integer. (str i)))))]
    (+ (* 10 (first numbers))
       (last numbers))))

(defn part1 [input]
  (->> input
       (map process-line)
       (reduce +)))

#_(part1 (h/parse-input 2023 1 "\n"))

(defn replacement [line]
  (reduce (fn [memo [text replace]]
            (string/replace memo text replace))
          line
          {"one" "o1ne"
           "two" "t2wo"
           "three" "th3ree"
           "four" "fo4ur"
           "five" "fi5ve"
           "six" "si6x"
           "seven" "se7ven"
           "eight" "eig8ht"
           "nine" "ni9ne"}))

(tests
 (replacement "eightwo") := "eig8ht2wo")

(defn part2 [input]
  (->> input
       (map replacement)
       (map process-line)
       (reduce +)))

#_(part2 (h/parse-input 2023 1 "\n"))

(tests
 (part1 (h/parse-input 2023 "1example" "\n"))
 := 142

 (part2 (h/parse-input 2023 "1example2" "\n"))
 := 281)
