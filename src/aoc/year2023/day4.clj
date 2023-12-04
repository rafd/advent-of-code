(ns aoc.year2023.day4
  (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [hyperfiddle.rcf :as r]
   [aoc.helpers :as h]))

(defn parse-line [line]
  (let [[_ id winning-numbers our-numbers] (re-matches #"Card +(\d+): ([0-9 ]+)\|([0-9 ]+)" line)]
    {:card-id (parse-long id)
     :winning-numbers (set (keep parse-long (string/split winning-numbers #" ")))
     :our-numbers (set (keep parse-long (string/split our-numbers #" ")))}))

(defn part1 [input]
  (->> input
       (map parse-line)
       (map (fn [{:keys [winning-numbers our-numbers]}]
              (let [matches (count (set/intersection winning-numbers our-numbers))]
                (if (= 0 matches)
                  0
                  (Math/pow 2 (dec (count (set/intersection winning-numbers our-numbers))))))))
       (reduce +)
       int))

(r/tests
 (part1 (h/parse-input 2023 "4example" "\n")) := 13)

#_(part1 (h/parse-input 2023 4 "\n"))

(defn part2 [input]
  (->> input
       (map parse-line)))

(r/tests
 (part2 (h/parse-input 2023 "4example" "\n")) := nil)

#_(part2 (h/parse-input 2023 1 "\n"))
