(ns aoc.year2024.day8
  (:require
   [clojure.string :as string]
   [clojure.math.combinatorics :as combo]
   [hyperfiddle.rcf :as r]
   [aoc.helpers :as h]))

(defn parse [input]
  (->> input
       string/split-lines
       (map-indexed (fn [y line]
                      (map-indexed (fn [x value]
                                     [[x y] value])
                                   line)))
       (apply concat)
       (remove (fn [[_ value]]
                 (= value \.)))
       (reduce (fn [memo [coordinate value]]
                 (update memo value (fnil conj #{}) coordinate)) {})))

(defn v- [a b]
  (mapv - a b))

(defn v+ [a b]
  (mapv + a b))

(defn part1 [input]
  (let [y-max (count (filter #{\newline} input))
        x-max (dec (.indexOf input "\n"))]
    (->> (parse input)
         ;; {\a #{[1 2] ...} ...}
         (mapcat (fn [[_ coordinates]]
                   (combo/combinations coordinates 2)))
         (mapcat (fn [[a b]]
                   (let [delta-v (v- a b)]
                     [(v+ a delta-v) (v- b delta-v)])))
         distinct
         (filter (fn [[y x]]
                   (and (<= 0 y y-max)
                        (<= 0 x x-max))))
         count)))

(r/tests
 (part1 (h/get-input 2024 "8example")) := 14
 (part1 (h/get-input 2024 8)) := 327)

(defn anti-nodes [start-v delta-v y-max x-max]
  (->> [delta-v (v- [0 0] delta-v)]
       (mapcat (fn [delta-v]
                 (take-while (fn [[y x]]
                               (and (<= 0 y y-max)
                                    (<= 0 x x-max)))
                             (iterate (partial v+ delta-v) start-v))))))

#_(anti-nodes [1 1] [1 1] 5 7)

(defn part2 [input]
  (let [y-max (count (filter #{\newline} input))
        x-max (dec (.indexOf input "\n"))]
    (->> (parse input)
         ;; {\a #{[1 2] ...} ...}
         (mapcat (fn [[_ coordinates]]
                   (combo/combinations coordinates 2)))
         (mapcat (fn [[a b]]
                   (let [delta-v (v- a b)]
                     (anti-nodes a delta-v y-max x-max))))
         distinct
         count)))

(r/tests
 (part2 (h/get-input 2024 "8example")) := 34
 (part2 (h/get-input 2024 8)) := 1233)

