(ns aoc.year2022.day15
 (:require
  [aoc.helpers :as h]
  [clojure.string :as string]
  [hyperfiddle.rcf :refer [tests]]
  [clojure.set :as set]))

(defn width-at-y
  [slice-y [sensor-x sensor-y] [beacon-x beacon-y]]
  (let [distance (+ (Math/abs ^java.lang.Integer (- sensor-x beacon-x))
                    (Math/abs ^java.lang.Integer (- sensor-y beacon-y)))
        leftover (- distance (Math/abs ^java.lang.Integer (- slice-y sensor-y)))]
    (set (range (- sensor-x leftover) (inc (+ sensor-x leftover))))))

(defn beaconless-points
  [slice-y [sensor-x sensor-y] [beacon-x beacon-y]]
  (disj (width-at-y slice-y [sensor-x sensor-y] [beacon-x beacon-y])
        (when (= beacon-y slice-y)
          beacon-x)))

(tests
 (beaconless-points 9 [0 0] [5 -6]) := #{-2 -1 0 1 2}
 (beaconless-points 30 [0 0] [5 -6]) := #{}
 (beaconless-points -9 [0 0] [5 -6]) := #{-2 -1 0 1 2}
 (beaconless-points 10 [1 1] [6 -5]) := #{-1 0 1 2 3}
 (beaconless-points 8 [-1 -1] [4 -7]) := #{-3 -2 -1 0 1}
 "beacon is not included"
 (beaconless-points -6 [0 0] [5 -6]) := #{-5 -4 -3 -2 -1 0 1 2 3 4}
 (beaconless-points 1 [0 0] [0 -1]) := #{0})

(defn parse [input]
  (->> input
       (h/rsplit "\n")
       (map (fn [line]
              (->> (re-matches #"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)" line)
                   rest
                   (map parse-long)
                   (partition 2))))))

;; subtract if beacon is on line


(defn part1 [slice-y input]
  (->> input
       parse
       (map (partial apply beaconless-points slice-y))
       (apply set/union)
       count))

(defn diagram [input]
  (let [pad 5
        sensors-and-beacons (parse input)
        min-x (-  (apply min (mapcat (fn [[[sensor-x _] [beacon-x _]]]
                                   [sensor-x beacon-x])
                                 sensors-and-beacons))
                  pad)
        max-x (+ (apply max (mapcat (fn [[[sensor-x _] [beacon-x _]]]
                                   [sensor-x beacon-x])
                                 sensors-and-beacons))
                 pad)
        min-y (- (apply min (mapcat (fn [[[_ sensor-y] [_ beacon-y]]]
                                   [sensor-y beacon-y])
                                 sensors-and-beacons))
                 pad)
        max-y (+ (apply max (mapcat (fn [[[_ sensor-y] [_ beacon-y]]]
                                   [sensor-y beacon-y])
                                 sensors-and-beacons))
                 pad)
        sensors (set (map first sensors-and-beacons))
        beacons (set (map second sensors-and-beacons))]
    (string/join "\n"
                 (for [y (range min-y (inc max-y))
                       :let [points (apply set/union (map (partial apply beaconless-points y) sensors-and-beacons))]]
                   (string/join ""
                                (for [x (range min-x (inc max-x))]
                                  (cond
                                    (contains? sensors [x y])
                                    "S"
                                    (contains? beacons [x y])
                                    "B"
                                    (contains? points x)
                                    "#"
                                    :else
                                    ".")))))))

#_(println (diagram (h/get-input 2022 "15example")))

(tests
 (part1 10 (h/get-input 2022 "15example")) := 26)

#_(part1 2000000 (h/get-input 2022 15))

(defn merge-ranges
  "Assumes they overlap"
  [[a b] [c d]]
  [(min a c) (max b d)])

(defn conj-ranges
  "Assumes ranges is sorted"
  [ranges new-range]
  (loop [new-range new-range
         out-ranges []
         ranges ranges]
    (let [[a b] (first ranges)
          [A B] new-range]
      (cond
        (empty? ranges)
        (conj out-ranges new-range)
        (< b A)
        (recur new-range (conj out-ranges (first ranges)) (rest ranges))
        (< B a)
        (concat out-ranges [new-range] ranges)
        :else
        (recur (merge-ranges new-range (first ranges)) out-ranges (rest ranges))))))

(tests
 (conj-ranges [] [0 10]) := [[0 10]]
 (conj-ranges [[1 2] [4 5] [7 8]] [0 10]) := [[0 10]]
 (conj-ranges [[1 2] [4 5] [7 8]] [3 4]) := [[1 2] [3 5] [7 8]]
 (conj-ranges [[1 2] [7 8]] [4 5]) := [[1 2] [4 5] [7 8]])

(defn merge-unsorted-ranges
  [unsorted-ranges]
  (->> unsorted-ranges
       sort
       (reduce conj-ranges [])))

(tests
 (merge-unsorted-ranges [[7 8] [1 2] [0 10] [4 5]]) := [[0 10]])

(defn range-at-y-fast
  [slice-y [sensor-x sensor-y] [beacon-x beacon-y]]
  (let [distance (+ (Math/abs ^java.lang.Integer (- sensor-x beacon-x))
                    (Math/abs ^java.lang.Integer  (- sensor-y beacon-y)))
        leftover (- distance (Math/abs ^java.lang.Integer (- slice-y sensor-y)))]
    (when (<= 0 leftover)
      [(- sensor-x leftover) (+ sensor-x leftover)])))

(tests
 (range-at-y-fast 9 [0 0] [5 -6]) := [-2 2]
 (range-at-y-fast 30 [0 0] [5 -6]) := nil
 (range-at-y-fast -9 [0 0] [5 -6]) := [-2 2]
 (range-at-y-fast 10 [1 1] [6 -5]) := [-1 3]
 (range-at-y-fast 8 [-1 -1] [4 -7]) := [-3 1]
 (range-at-y-fast 1 [0 0] [0 -1]) := [0 0])

(defn missing-number [min-x max-x points]
  (let [sum (reduce + points)
        expected-sum (* (+ max-x min-x)
                        (/ (- max-x (dec min-x)) 2))]
    (int (- expected-sum sum))))

(tests
 (missing-number 1 10 #{1 2 3 4 5 6 8 9 10}) := 7
 (missing-number -2 2 #{-2 -1 0 2}) := 1
 (missing-number -2 2 #{-2 -1 1 2}) := 0
 (missing-number -3 -1 #{-3 -1}) := -2)

(defn missing-beacon [min-y max-y sensors-and-beacons]
  (let [y (volatile! min-y)
        result (volatile! nil)]
    (while (nil? @result)
      (when (= 0 (mod @y 500000))
        (println @y))
      (let [ranges (merge-unsorted-ranges (remove nil? (map (partial apply range-at-y-fast @y) sensors-and-beacons)))]
        (if (= 2 (count ranges))
          (vreset! result [(inc (last (first ranges))) @y])
          (vswap! y inc))))
    @result))

(defn tuning-frequency [[x y]]
  (+ (* x 4000000) y))

(defn part2 [min-y max-y input]
  (->> input
       parse
       (missing-beacon min-y max-y)
       (tuning-frequency)))

(tests
 (missing-beacon 0 20 (parse (h/get-input 2022 "15example"))) := [14 11]
 (part2 0 20 (h/get-input 2022 "15example")) := 56000011)

#_(part2 0 4000000 (h/get-input 2022 15))
