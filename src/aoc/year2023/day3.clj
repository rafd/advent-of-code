(ns aoc.year2023.day3
  (:require
   [hyperfiddle.rcf :as r]
   [aoc.helpers :as h]))

(defn char-symbol? [char]
  (not (re-matches #"\d|\." (str char))))

(defn part1 [input]
  (let [parts-grid (->> input
                        (mapv (fn [line]
                                (->> (re-seq #"\d+|." line)
                                     (mapcat (fn [num-string-or-dot]
                                               (if-let [number (parse-long num-string-or-dot)]
                                                 (repeat (count num-string-or-dot)
                                                         {:part/id (gensym)
                                                          :part/number number})
                                                 [nil])))
                                     vec))))
        grid (->> input (mapv vec))
        symbol-coordinates (for [x (range 0 (count input))
                                 y (range 0 (count (first input)))
                                 :when (char-symbol? (get-in grid [x y]))]
                             [x y])]
    (->> symbol-coordinates
         (mapcat (fn [[x y]]
                   (->> [[-1 -1] [-1 0] [-1 1]
                         [0 -1]  [0 0]  [0 1]
                         [1 -1]  [1 0]  [1 1]]
                        (keep (fn [[dx dy]]
                               (get-in parts-grid [(+ x dx) (+ y dy)]))))))
         set
         (map :part/number)
         (reduce +))))

(r/tests
 (part1 (h/parse-input 2023 "3example" "\n")) := 4361)

#_(part1 (h/parse-input 2023 3 "\n")) ;; 536202

(defn part2 [input]
  (let [parts-grid (->> input
                        (mapv (fn [line]
                                (->> (re-seq #"\d+|." line)
                                     (mapcat (fn [num-string-or-dot]
                                               (if-let [number (parse-long num-string-or-dot)]
                                                 (repeat (count num-string-or-dot)
                                                         {:part/id (gensym)
                                                          :part/number number})
                                                 [nil])))
                                     vec))))
        grid (->> input (mapv vec))
        symbol-coordinates (for [x (range 0 (count input))
                                 y (range 0 (count (first input)))
                                 :when (char-symbol? (get-in grid [x y]))]
                             [x y])]
    (->> symbol-coordinates
         (filter (fn [[x y]]
                   (= \* (get-in grid [x y]))))
         (map (fn [[x y]]
                (->> [[-1 -1] [-1 0] [-1 1]
                      [0 -1]  [0 0]  [0 1]
                      [1 -1]  [1 0]  [1 1]]
                     (keep (fn [[dx dy]]
                            (get-in parts-grid [(+ x dx) (+ y dy)])))
                     set)))
         (filter (fn [found-parts]
                   (= 2 (count found-parts))))
         (map (fn [pair]
                (* (:part/number (first pair))
                   (:part/number (second pair)))))
         (reduce +))))

(r/tests
 (part2 (h/parse-input 2023 "3example" "\n")) := 467835)

#_(part2 (h/parse-input 2023 3 "\n")) ;; 78272573
