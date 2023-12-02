(ns aoc.year2023.day2
  (:require
   [clojure.string :as string]
   [hyperfiddle.rcf :as r]
   [aoc.helpers :as h]))

(def bag
  {:red 12
   :green 13
   :blue 14})

(defn parse-line [line]
  (let [[_ game-id draws] (re-matches #"Game (\d+): (.*)" line)]
    [(Integer/parseInt game-id)
     (->> (string/split draws #";")
          (map (fn [draw-string]
                 (->> (string/split draw-string #",")
                      (map string/trim)
                      (map (fn [color-and-count-string]
                             (let [[_ count color] (re-matches #"(\d+) ([a-z]+)" color-and-count-string)]
                               [(keyword color) (Integer/parseInt count)])))
                      (into {})))))]))

(defn eligible? [bag draw]
  (every? (fn [[color count]]
            (<= count (bag color))) draw))

(r/tests
 (eligible? {:red 1} {:red 1}) := true
 (eligible? {:red 1} {:red 2}) := false)

(defn part1 [input]
  (->> input
       (map parse-line)
       (filter (fn [[_game-id draws]]
                 (every? (partial eligible? bag) draws)))
       (map (fn [[game-id _]]
              game-id))
       (reduce +)))

(r/tests
 (part1 (h/parse-input 2023 "2example" "\n")) := 8)

#_(part1 (h/parse-input 2023 2 "\n"))

(defn minimum-bag [draws]
  (apply merge-with max draws))

#_(minimum-bag [{:red 1 :blue 2} {:red 2}]) ;; 2164

(defn power [bag]
  ;; assuming no color can have 0 count
  (apply * (vals bag)))

(defn part2 [input]
  (->> input
       (map parse-line)
       (map (fn [[_game-id draws]]
              (minimum-bag draws)))
       (map power)
       (reduce +)))

(r/tests
 (part2 (h/parse-input 2023 "2example" "\n")) := 2286)

#_(part2 (h/parse-input 2023 2 "\n")) ;; 66929
