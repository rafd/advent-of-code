(ns aoc.year2024.day11
  (:require
   [clojure.string :as string]
   [hyperfiddle.rcf :as r]
   [aoc.helpers :as h]))

(defn blink [stones]
  (->> stones
       (mapcat (fn [stone]
                 (if (= 0 stone)
                   [1]
                   (let [stone-string (str stone)
                         half-length (quot (count stone-string) 2)]
                     (if (even? (count stone-string))
                       [(parse-long (subs stone-string 0 half-length))
                        (parse-long (subs stone-string half-length))]
                       [(* stone 2024)])))))))

(defn solve [input n]
  (let [stones (->> (string/split input #" ")
                    (map parse-long))]
    (count (reduce (fn [acc _]
                     (blink acc))
                   stones
                   (range 0 n)))))

(defn part1 [input]
  (solve input 25))

(r/tests
 (part1 (h/get-input 2024 "11example")) := 55312)

#_(part1 (h/get-input 2024 11)) := 217812
