(ns aoc.year2021.day13
  (:require
    [aoc.helpers :refer [parse-input]]
    [clojure.string :as string]))

(defn fold [points {:keys [fold-axis fold-value]}]
  (->> points
       (map (fn [point]
             (if (< fold-value (get point fold-axis))
               (update point fold-axis (fn [coordinate]
                                        (- (* 2 fold-value) coordinate)))
               point)))
       set))

(defn print-grid [points]
  (->> (for [y (range 0 (inc (apply max (map :y points))))]
         (for [x (range 0 (inc (apply max (map :x points))))]
          (if (contains? points {:x x :y y})
           "█"
           " ")))
      (map #(string/join "" %))
      (string/join "\n")))

#_(let [[points instructions] (parse-input 2021 "13" "\n\n")
        points (->> (string/split points #"\n")
                    (map (fn [point-string]
                          (let [[x y] (mapv #(Integer. %) (string/split point-string #","))]
                            {:x x :y y})))
                    set)
        instructions (->> (string/split instructions #"\n")
                          (map (fn [instruction-string]
                                (let [[_ axis value] (re-find #"fold along (x|y)=(\d+)" instruction-string)]
                                 {:fold-axis (keyword axis)
                                  :fold-value (Integer. value)}))))]
    ;; part 1
    #_(count (fold points (first instructions)))
    ;; part 2
    (println (print-grid (reduce fold points instructions))))
