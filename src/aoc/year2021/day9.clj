(ns aoc.year2021.day9
  (:require
    [aoc.helpers :refer [parse-input]]
    [clojure.set :as set]
    [clojure.string :as string]))

(defn get-height [grid x y]
  (try
    (get-in grid [x y])
    (catch Exception _
      nil)))

(defn find-low-cells [grid]
  (->> (for [x (range 0 (count grid))
             y (range 0 (count (first grid)))
             :let [height (get-height grid x y)
                   up-height (get-height grid (dec x) y)
                   down-height (get-height grid (inc x) y)
                   right-height (get-height grid x (inc y))
                   left-height (get-height grid x (dec y))]]
         (if (every? (partial < height)
                     (remove nil? [up-height down-height
                                   right-height left-height]))
            [x y]
            nil))
       (remove nil?)))


;; part 1

#_(let [grid (->> (parse-input 2021 9 "\n")
                  (mapv (fn [row] (mapv #(Integer. (str %)) row))))]
    (->> (find-low-cells grid)
         (map (fn [[x y]] (get-height grid x y)))
         (map inc)
         (reduce +)))

;; part 2

(defn invalid-basin-cell? [grid basin-cells x y]
  (let [height (get-height grid x y)]
    (or (nil? height)
        (= 9 height)
        (contains? basin-cells [x y]))))

(defn expand-basin [grid basin-cells x y]
 (if (invalid-basin-cell? grid basin-cells x y)
   basin-cells
   (as-> basin-cells $
         (conj $ [x y])
         (expand-basin grid $ (dec x) y)   ;; recur up
         (expand-basin grid $ (inc x) y)   ;; recur down
         (expand-basin grid $ x (inc y))   ;; recur right
         (expand-basin grid $ x (dec y))))) ;; recur left

#_(let [grid (->> (parse-input 2021 "9" "\n")
                  (mapv (fn [row] (mapv #(Integer. (str %)) row))))
        low-cells (find-low-cells grid)
        basins (map (fn [[x y]] (expand-basin grid #{} x y)) low-cells)]
   (->> (map count basins)
        sort
        reverse
        (take 3)
        (reduce *)))
