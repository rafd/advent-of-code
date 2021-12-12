(ns aoc.year2021.day4
  (:require
    [clojure.string :as string]
    [aoc.helpers :refer [parse-input]]))

(defn parse-numbers-and-boards [input]
  (let [numbers (->> (string/split (first input) #",")
                     (map #(Integer. %)))
        boards (->> (rest input)
                    (map (fn [board]
                           (->> (string/split board #"\n")
                                (map (fn [line] (->> (string/split (string/trim line) #" +")
                                                     (map #(Integer. %)))))))))]
       [numbers boards]))

(defn transpose [x]
  (apply map list x))

(defn row-winner? [board]
  (boolean (some (fn [row] (apply = :x row)) board)))

(defn column-winner? [board]
  (row-winner? (transpose board)))

(defn diagonal-winner-down-right? [board]
  (cond
    (= board [])
    true

    (= :x (ffirst board))
    (recur (map rest (rest board)))

    :else
    false))

(defn diagonal-winner? [board]
  (or (diagonal-winner-down-right? board)
      (diagonal-winner-down-right? (transpose board))))

(defn is-winner? [board]
  (or (row-winner? board)
      (column-winner? board)
      #_(diagonal-winner? board))) ;; diagonals don't count, sad!

(defn mark [target board]
  (map (fn [row] (map (fn [number] (if (= number target) :x number)) row)) board))

(defn calculate-score [board number]
  (->> board
       flatten
       (remove #{:x})
       (apply +)
       (* number)))

;; day 4 part 1

#_(let [[numbers boards] (parse-numbers-and-boards (parse-input 2021 4 "\n\n"))
        [winning-board winning-number] (reduce (fn [boards number]
                                                 (let [new-boards (map (partial mark number) boards)]
                                                   (if-let [winning-board (first (filter is-winner? new-boards))]
                                                     (reduced [winning-board number])
                                                     new-boards)))
                                               boards
                                               numbers)]
       (calculate-score winning-board winning-number))

;; day 4 part 2

#_(let [[numbers boards] (parse-numbers-and-boards (parse-input 2021 4 "\n\n"))
        [winning-board winning-number] (reduce (fn [boards number]
                                                 (let [new-boards (remove is-winner? (map (partial mark number) boards))]
                                                   (if (empty? new-boards)
                                                     (reduced [(mark number (first boards)) number])
                                                     new-boards)))
                                               boards
                                               numbers)]
       (calculate-score winning-board winning-number))
