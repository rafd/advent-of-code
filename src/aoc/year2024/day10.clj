(ns aoc.year2024.day10
  (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [hyperfiddle.rcf :as r]
   [aoc.helpers :as h]))

(defn part1 [input]
  (let [grid (->> input
                  string/split-lines
                  (mapv (fn [line]
                          (mapv parse-long (string/split line #"")))))
        ;; copy grid, overwrite with 0s
        score-grid (atom (->> grid
                              (mapv (fn [row]
                                      (mapv (constantly #{}) row)))))]

    (doseq [x (range (count grid))
            y (range (count (first grid)))]
      (when (= 9 (get-in grid [x y]))
        (swap! score-grid update-in [x y] conj [x y])))

    (doseq [n (range 8 -1 -1)]
      (doseq [x (range (count grid))
              y (range (count (first grid)))]
        (when (= n (get-in grid [x y]))
          (swap! score-grid assoc-in [x y]
                 (->> [[x (inc y)]
                       [x (dec y)]
                       [(inc x) y]
                       [(dec x) y]]
                      (map (fn [coord]
                             (if (= (inc n) (get-in grid coord))
                               (get-in @score-grid coord)
                               #{})))
                      (apply set/union))))))

    (->> (for [x (range (count grid))
               y (range (count (first grid)))]
           (when (= 0 (get-in grid [x y]))
             (count (get-in @score-grid [x y]))))
         flatten
         (remove nil?)
         (apply +))))

(r/tests
 (part1 (h/get-input 2024 "10example")) := 36
 (part1 (h/get-input 2024 10)) := 667)

(defn part2 [input]
  (let [grid (->> input
                  string/split-lines
                  (mapv (fn [line]
                          (mapv parse-long (string/split line #"")))))
        ;; copy grid, overwrite with 0s
        score-grid (atom (->> grid
                              (mapv (fn [row]
                                      (mapv (constantly 0) row)))))]

    (doseq [x (range (count grid))
            y (range (count (first grid)))]
      (when (= 9 (get-in grid [x y]))
        (swap! score-grid update-in [x y] inc)))

    (doseq [n (range 8 -1 -1)]
      (doseq [x (range (count grid))
              y (range (count (first grid)))]
        (when (= n (get-in grid [x y]))
          (swap! score-grid assoc-in [x y]
                 (->> [[x (inc y)]
                       [x (dec y)]
                       [(inc x) y]
                       [(dec x) y]]
                      (map (fn [coord]
                             (if (= (inc n) (get-in grid coord))
                               (get-in @score-grid coord)
                               0)))
                      (apply +))))))

    (->> (for [x (range (count grid))
               y (range (count (first grid)))]
           (when (= 0 (get-in grid [x y]))
             (get-in @score-grid [x y])))
         flatten
         (remove nil?)
         (apply +))))

(r/tests
 (part2 (h/get-input 2024 "10example")) := 81
 (part2 (h/get-input 2024 10)) := 1344)
