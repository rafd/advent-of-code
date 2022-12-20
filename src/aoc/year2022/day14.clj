(ns aoc.year2022.day14
 (:require
  [aoc.helpers :as h]
  [hyperfiddle.rcf :refer [tests]]
  [com.rpl.specter :as x]
  [clojure.set :as set]))

(defn points-from-ends [[[x1 y1] [x2 y2]]]
  (set (for [x (range (min x1 x2) (inc (max x1 x2)))
             y (range (min y1 y2) (inc (max y1 y2)))]
         [x y])))

(tests
 (points-from-ends [[0 0] [0 2]]) := #{[0 0] [0 1] [0 2]}
 (points-from-ends [[2 0] [0 0]]) := #{[0 0] [1 0] [2 0]})

(defn parse [input]
  (->> input
       (h/rsplit "\n")
       (map (fn [line]
              (map rest (re-seq #"(\d+),(\d+)" line))))
       (x/transform [x/ALL x/ALL x/ALL] #(Integer/parseInt %))
       (map (fn [line-pairs]
              (->> line-pairs
                   (partition 2 1)
                   (map points-from-ends)
                   (apply set/union))))
       (apply set/union)))

(defn down [[x y]]
  [x (inc y)])

(defn down-left [[x y]]
  [(dec x) (inc y)])

(defn down-right [[x y]]
  [(inc x) (inc y)])

(defn next-sand-location
  [current-sand-location rocks sand floor]
  (let [occupied? (fn [potential-location]
                   (or (contains? rocks potential-location)
                       (contains? sand potential-location)))]
    (->> [(down current-sand-location)
          (down-left current-sand-location)
          (down-right current-sand-location)]
         (some (fn [[_x y :as potential-location]]
                 (when (and (< y floor)
                            (not (occupied? potential-location)))
                   potential-location))))))

(tests
 (next-sand-location [10 0] #{} #{} ##Inf) := [10 1]
 (next-sand-location [10 0] #{[10 1]} #{} ##Inf) := [9 1]
 (next-sand-location [10 0] #{[10 1] [9 1]} #{} ##Inf) := [11 1]
 (next-sand-location [10 0] #{[10 1] [9 1] [11 1]} #{} ##Inf) := nil)

(defn part1 [input]
  (let [sand-source [500 0]
        rocks (parse input)
        abyss-y (apply max (map second rocks))]
    (loop [current-sand sand-source
           stable-sand #{}]
      (if (< abyss-y (second current-sand))
        (count stable-sand)
        (if-let [next-location (next-sand-location current-sand rocks stable-sand ##Inf)]
          (recur next-location stable-sand)
          (recur sand-source (conj stable-sand current-sand)))))))

(tests
 (part1 (h/get-input 2022 "14example")) := 24)

#_(part1 (h/get-input 2022 14))

(defn part2 [input]
  (let [sand-source [500 0]
        rocks (parse input)
        abyss-y (+ 2 (apply max (map second rocks)))]
    (loop [current-sand sand-source
           stable-sand #{}]
      (if (contains? stable-sand sand-source)
        (count stable-sand)
        (if-let [next-location (next-sand-location current-sand rocks stable-sand abyss-y)]
          (recur next-location stable-sand)
          (recur sand-source (conj stable-sand current-sand)))))))

(tests
 (part2 (h/get-input 2022 "14example")) := 93)

#_(part2 (h/get-input 2022 14))
