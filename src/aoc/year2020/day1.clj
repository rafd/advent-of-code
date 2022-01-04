(ns aoc.year2020.day1
 (:require
   [clojure.string :as string]
   [aoc.helpers :as helpers]))

(defn part1 [input]
  (let [numbers (->> (string/split input #"\n")
                     (map #(Integer. %))
                     set)
        a (->> numbers
               (filter (fn [x]
                        (contains? numbers (- 2020 x))))
               first)]
    (* a (- 2020 a))))

#_(part1 (helpers/get-input 2020 1))

(defn part2 [input]
  (let [numbers (->> (string/split input #"\n")
                     (mapv #(Integer. %)))]
    (->> (for [i (range 0 (count numbers))
               j (range i (count numbers))
               k (range j (count numbers))]
          [(numbers i) (numbers j) (numbers k)])
         (filter (fn [[i j k]]
                  (= 2020 (+ i j k))))
         first
         (apply *))))

#_(part2 (helpers/get-input 2020 1))
