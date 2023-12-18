(ns aoc.year2021.day7
  (:require
    [aoc.helpers :refer [parse-input]]))

;; day 7 part 1
#_(let [crabs (->> (parse-input 2021 7 ",")
                   (map parse-long)
                   (sort))
        median (nth crabs (int (/ (count crabs)
                                  2)))
        fuel (->> crabs
                  (map #(Math/abs (- % median)))
                  (reduce +))]
       fuel)

;; day 7 part 2
#_(let [crabs (->> (parse-input 2021 7 ",")
                   #_(string/split "16,1,2,0,4,2,7,1,2,14" #",")
                   (map parse-long))
        avg (/ (reduce + crabs)
               (count crabs))
        positions [(Math/floor avg) (Math/ceil avg)]
        cost (fn [a x]
               (* (Math/abs (- a x)) (+ (Math/abs (- a x)) 1) 0.5))
        fuel-costs (map (fn [proposed-position]
                         (reduce + (map (partial cost proposed-position) crabs)))
                        positions)]
    (apply min fuel-costs))

;; day 7 part 2 approach 2
#_(time
   (let [crabs (->> (parse-input 2021 7 ",")
                    (map parse-long))
         positions (range (apply min crabs) (inc (apply max crabs)))
         cost (fn [a x]
                (* (Math/abs (- a x)) (+ (Math/abs (- a x)) 1) 0.5))
         fuel-costs (map (fn [proposed-position]
                          (reduce + (map (partial cost proposed-position) crabs)))
                         positions)]
     (apply min fuel-costs)))
