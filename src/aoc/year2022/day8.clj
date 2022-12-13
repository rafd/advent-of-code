(ns aoc.year2022.day8
 (:require
  [aoc.helpers :as h]
  [hyperfiddle.rcf :refer [tests]]
  [clojure.string :as string]))

(defn visibility1d [heights]
  (->> heights
       (reduce (fn [memo height]
                 (-> memo
                     (update :visibilities conj (< (:tallest-so-far memo) height))
                     (update :tallest-so-far max height)))
               {:tallest-so-far -1
                :visibilities []})
       :visibilities))

(tests
 (visibility1d [1 2 3 2 1 7]) := [true true true false false true]
 (visibility1d [7 1 2 3 2 1]) := [true false false false false false])

(defn visibility2d
  [heights-grid]
  (->> heights-grid
       (map visibility1d)))

(tests
 (visibility2d [[1 2] [4 3]]) := [[true true] [true false]])

(defn transpose [m]
  (apply mapv vector m))

(def perspective-fns
  [identity
   #(mapv (comp vec reverse) %)
   #(transpose %)
   #(mapv (comp vec reverse) (transpose %))])

(def reverse-perspective-fns
  [identity
   #(mapv (comp vec reverse) %)
   #(transpose %)
   #(transpose (mapv (comp vec reverse) %))])

(defn perspectives [grid]
  (map #(% grid) perspective-fns))

(tests
 "Reverses correctly"
 (map (fn [a b] (a (b [[1 2] [3 4]]))) perspective-fns reverse-perspective-fns)
 := (repeat 4 [[1 2] [3 4]]))

(tests
 (perspectives [[1 2] [3 4]]) := [[[1 2]
                                   [3 4]]
                                  [[2 1]
                                   [4 3]]
                                  [[1 3]
                                   [2 4]]
                                  [[3 1]
                                   [4 2]]])

(defn merge-grids [grids]
  (apply map (fn [& items]
               (reduce #(or %1 %2) items)) grids))

(defn count-visible [grid]
  (->> grid
       perspectives
       (map visibility2d)
       (map (fn [f x] (f x)) reverse-perspective-fns)
       (map flatten)
       merge-grids
       (filter true?)
       count))

(defn part1 [input]
  (->> (string/split input #"\n")
       (map (fn [row] (map #(Integer. ^java.lang.String %) (string/split row #""))))
       count-visible))

(tests
 (count-visible [[1 2]
                 [3 4]]) := 4
 (part1 (h/get-input 2022 "8example")) := 21)

#_(part1 (h/get-input 2022 8))
