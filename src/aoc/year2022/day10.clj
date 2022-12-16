(ns aoc.year2022.day10
 (:require
  [aoc.helpers :as h]
  [hyperfiddle.rcf :refer [tests]]
  [clojure.string :as string]))

(defn parse [input]
  (->> input
       (reduce (fn [memo instruction]
                 (-> memo
                     (conj (last memo))
                     (cond->
                      (not= "noop" instruction)
                      (conj (+ (last memo) (Integer/parseInt ^java.lang.String (last (string/split instruction #" "))))))))
               [1 1])))

(defn part1 [input]
  (->> input
       parse
       (map-indexed vector)
       (drop 20)
       (take-nth 40)
       (map (partial apply *))
       (reduce +)))

(tests
 (part1 (h/parse-input 2022 "10example" "\n")) := 13140)

#_(part1 (h/parse-input 2022 10 "\n"))

(defn part2 [input]
  (->> input
       parse
       (drop 1)
       (partition 40)
       (map (fn [line]
              (map-indexed vector line)))
       (map (fn [line]
              (map (fn [[draw-position sprite-middle-position]]
                     (println draw-position sprite-middle-position)
                     (if (<= (dec sprite-middle-position) draw-position (inc sprite-middle-position))
                       "#"
                       ".")) line)))
       (map (fn [line]
              (string/join "" line)))
       (string/join "\n")))

#_(spit "day10out.txt" (part2 (h/parse-input 2022 "10example" "\n")))
#_(spit "day10out.txt" (part2 (h/parse-input 2022 10 "\n")))

