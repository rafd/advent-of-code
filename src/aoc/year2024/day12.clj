(ns aoc.year2024.day12
  (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [hyperfiddle.rcf :as r]
   [aoc.helpers :as h]))

(defn find-path [grid plant to-visit-atom [i j]]
  (let [plant-matches? (= plant (get-in grid [i j]))
        visited? (not (contains? @to-visit-atom [i j]))]
    (swap! to-visit-atom disj [i j])
    (cond
      (and plant-matches? visited?)
      []

      (and plant-matches? (not visited?))
      (concat
       [[i j]]
       (find-path grid plant to-visit-atom [(inc i) j])
       (find-path grid plant to-visit-atom [(dec i) j])
       (find-path grid plant to-visit-atom [i (inc j)])
       (find-path grid plant to-visit-atom [i (dec j)]))

      (and (not plant-matches?) visited?)
      [:fence]

      (and (not plant-matches?) (not visited?))
      [:fence])))

(r/tests
 (sort-by str (find-path [[\x \x] [\x \o]] \x (atom #{[0 0] [0 1] [1 0] [1 1]}) [0 0]))
 := [:fence :fence :fence :fence :fence :fence :fence :fence [0 0] [0 1] [1 0]]
 (sort-by str (find-path [[\x \x] [\x \x]] \x (atom #{[0 0] [0 1] [1 0] [1 1]}) [0 0]))
 := [:fence :fence :fence :fence :fence :fence :fence :fence [0 0] [0 1] [1 0] [1 1]])

(defn part1 [input]
  (let [grid (->> input
                  string/split-lines
                  (mapv (fn [line]
                          (vec line))))
        to-visit (set (for [i (range 0 (count grid))
                            j (range 0 (count (first grid)))]
                        [i j]))]
    (loop [to-visit to-visit
           total-price 0]
      (if (seq to-visit)
        (let [location (first to-visit)
              path (find-path grid (get-in grid location) (atom to-visit) location)
              area (->> path
                        (remove #{:fence})
                        count)
              perimeter (->> path
                             (filter #{:fence})
                             count)
              price (* area perimeter)]
          (recur (set/difference (disj to-visit location)
                                 (set path))
                 (+ total-price price)))
        total-price))))

(r/tests
 (part1 (h/get-input 2024 "12example")) := 1930
 (part1 (h/get-input 2024 12)) := 1370100)
