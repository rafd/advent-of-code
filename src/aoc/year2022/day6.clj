(ns aoc.year2022.day6
  (:require
   [aoc.helpers :as h]
   [hyperfiddle.rcf :refer [tests]]))

(defn parse [window-size input]
  (->> input
       (partition window-size 1)
       (map set)
       (map-indexed vector)
       (some (fn [[index items]]
               (when (= window-size (count items))
                 index)))
       (+ window-size)))

(defn part1 [input]
  (parse 4 input))

(tests
 (part1 "mjqjpqmgbljsphdztnvjfqwrcgsmlb") := 7
 (part1 "bvwbjplbgvbhsrlpgdmjqwftvncz") := 5
 (part1 "nppdvjthqldpwncqszvftbrmjlhg") := 6
 (part1 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") := 10
 (part1 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") := 11)

#_(part1 (h/get-input 2022 6))

(defn part2 [input]
  (parse 14 input))

(tests
 (part2 "mjqjpqmgbljsphdztnvjfqwrcgsmlb") := 19
 (part2 "bvwbjplbgvbhsrlpgdmjqwftvncz") := 23
 (part2 "nppdvjthqldpwncqszvftbrmjlhg") := 23
 (part2 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") := 29
 (part2 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") := 26)

#_(part2 (h/get-input 2022 6))
