(ns aoc.year2023.day3
  (:require
   [hyperfiddle.rcf :as r]
   [aoc.helpers :as h]))

(defn char-symbol? [char]
  (not (re-matches #"\d|\." (str char))))

(defn ->parts-grid [input]
  (->> input
       (mapv (fn [line]
               (->> (re-seq #"\d+|." line)
                    (mapcat (fn [string]
                              (if-let [number (parse-long string)]
                                (repeat (count string)
                                        {:part/id (gensym)
                                         :part/number number})
                                [nil])))
                    vec)))))

(defn ->symbols [input]
  (let [grid (->> input (mapv vec))]
    (for [x (range 0 (count input))
          y (range 0 (count (first input)))
          :let [value (get-in grid [x y])]
          :when (char-symbol? value)]
      {:symbol/value value
       :symbol/x x
       :symbol/y y})))

(defn neighboring-parts
  [parts-grid x y]
  (->> [[-1 -1] [-1 0] [-1 1]
        [ 0 -1]        [ 0 1]
        [ 1 -1] [ 1 0] [ 1 1]]
       (keep (fn [[dx dy]]
               (get-in parts-grid [(+ x dx) (+ y dy)])))
       set))

(defn part1 [input]
  (let [parts-grid (->parts-grid input)
        symbols (->symbols input)]
    (->> symbols
         (mapcat (fn [{:symbol/keys [x y]}]
                   (neighboring-parts parts-grid x y)))
         set
         (map :part/number)
         (reduce +))))

(r/tests
 (part1 (h/parse-input 2023 "3example" "\n")) := 4361)

#_(part1 (h/parse-input 2023 3 "\n")) ;; 536202

(defn part2 [input]
  (let [parts-grid (->parts-grid input)
        symbols (->symbols input)]
    (->> symbols
         (filter (fn [{:symbol/keys [value]}]
                   (= \* value)))
         (map (fn [{:symbol/keys [x y]}]
                (neighboring-parts parts-grid x y)))
         (filter (fn [found-parts]
                   (= 2 (count found-parts))))
         (map (fn [pair]
                (* (:part/number (first pair))
                   (:part/number (second pair)))))
         (reduce +))))

(r/tests
 (part2 (h/parse-input 2023 "3example" "\n")) := 467835)

#_(part2 (h/parse-input 2023 3 "\n")) ;; 78272573
