(ns aoc.year2022.day15
 (:require
  [aoc.helpers :as h]
  [clojure.string :as string]
  [hyperfiddle.rcf :refer [tests]]
  [clojure.set :as set]))

(defn width-at-y
  [slice-y [sensor-x sensor-y] [beacon-x beacon-y]]
  (let [distance (+ (Math/abs (- sensor-x beacon-x))
                    (Math/abs (- sensor-y beacon-y)))
        leftover (- distance (Math/abs (- slice-y sensor-y)))]
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
                   (map #(Integer/parseInt %))
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
