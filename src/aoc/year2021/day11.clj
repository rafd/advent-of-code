(ns aoc.year2021.day11
  (:require
    [aoc.helpers :refer [parse-input]]))

(def flash-threshold 10)

(def direction-vectors
  [[1 0] [0 1] [-1 0] [0 -1] [1 1] [1 -1] [-1 1] [-1 -1]])

(defn update-all [grid f]
  (mapv #(mapv f %) grid))

(defn neighboring-coordinates [grid [x y]]
  (let [max-x (dec (count grid))
        max-y (dec (count (first grid)))]
    (->> direction-vectors
         (map (fn [[x' y']]
                [(+ x x') (+ y y')]))
         (filter (fn [[x y]]
                   (and (<= 0 x max-x)
                        (<= 0 y max-y)))))))

#_(neighboring-coordinates [[0 1 2] [3 4 5] [6 7 8]] [1 1])

(defn add-energy [input-grid coordinate]
  (let [grid (update-in input-grid coordinate inc)]
   (if (= flash-threshold (get-in grid coordinate))
     (reduce (fn [grid neighboring-coordinate]
               (add-energy grid neighboring-coordinate))
             grid
             (neighboring-coordinates grid coordinate))
     grid)))

#_(add-energy [[9 0 0] [0 0 0] [0 0 0]] [0 0])
#_(add-energy [[10 1 0] [1 1 0] [0 0 0]] [1 0])

(defn apply-step [grid]
  (as-> grid $
        ;; start a recursion for every octopus
        (reduce add-energy $ (for [x (range (count grid))
                                   y (range (count (first grid)))]
                               [x y]))
        ;; set any number greater-or-equal flash-threshold to 0
        (update-all $ (fn [energy]
                        (if (<= flash-threshold energy)
                         0
                         energy)))))

#_(apply-step [[8 0 0] [0 0 0] [0 0 0]])

;; part 1

(defn count-flashes [grid]
  (->> grid
       flatten
       (filter zero?)
       count))

#_(let [initial-grid (-> (parse-input 2021 "11" "\n")
                         (update-all #(parse-long (str %))))]
    (->> (reductions
           (fn [grid _]
             (apply-step grid))
           initial-grid
           (range 100))
         rest
         (map count-flashes)
         (reduce +)))

;; part 2

#_(let [initial-grid (-> (parse-input 2021 "11" "\n")
                         (update-all #(parse-long (str %))))]
    (loop [grid initial-grid
           count 0]
      (if (every? zero? (flatten grid))
        count
        (recur (apply-step grid)
               (inc count)))))
