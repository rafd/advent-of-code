(ns aoc.year2018.day2
 (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [clojure.math.combinatorics :as combo]
   [aoc.helpers :as helpers]))

(defn part1 [input]
  (let [freqs (->> input
                   string/split-lines
                   (map frequencies)
                   (map set/map-invert))]
    (* (->> freqs
            (filter (fn [f]
                     (contains? f 2)))
            count)
       (->> freqs
            (filter (fn [f]
                     (contains? f 3)))
            count))))

#_(part1 (helpers/get-input 2018 2))

(defn string-distance [a b]
 (count (remove true? (map = a b))))

#_(string-distance "aaa" "aaa")
#_(string-distance "aab" "aaa")

(defn part2 [input]
  (->> (combo/combinations (string/split-lines input) 2)
       (filter (fn [[a b]]
                (= 1 (string-distance a b))))
       first
       ((fn [[a b]]
         (apply str (map first (filter (partial apply =) (map vector a b))))))))


#_(part2 (helpers/get-input 2018 2))
