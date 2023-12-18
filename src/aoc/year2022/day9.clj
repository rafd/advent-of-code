(ns aoc.year2022.day9
  (:require
   [aoc.helpers :as h]
   [hyperfiddle.rcf :refer [tests]]
   [clojure.string :as string]))

(def transforms
  {"U" [0 1]
   "D" [0 -1]
   "R" [1 0]
   "L" [-1 0]})

(defn length [[x y]]
  (Math/sqrt (+ (Math/pow x 2)
                (Math/pow y 2))))

(defn normalize [^java.lang.Integer x]
  (if (zero? x) 0 (/ x (Math/abs x))))

(defn part1v0 [input]
  (->> input
       (map (partial h/rsplit " "))
       (mapcat (fn [[direction repeat-string]]
                 (repeat (parse-long repeat-string) direction)))
       ;; ("R" "L" ...)
       (reductions (fn [memo direction]

                     (let [new-head (mapv + (:head memo) (transforms direction))
                           delta (mapv - new-head (:tail memo))
                           new-tail (if (<= 2 (length delta))
                                      (mapv + (mapv normalize delta) (:tail memo))
                                      (:tail memo))]
                       {:head new-head
                        :tail new-tail}))
                   {:head [0 0]
                    :tail [0 0]})
       (map :tail)
       set
       count))

(defn part1 [input]
  (->> input
       (map (partial h/rsplit " "))
       (mapcat (fn [[direction repeat-string]]
                 (repeat (parse-long repeat-string) direction)))
       ;; ("R" "L" ...)
       (reductions (fn [memo direction]
                     (mapv + memo (transforms direction)))
                   [0 0])
       (reductions (fn [tail head]
                     (let [delta (mapv - head tail)]
                       (if (<= 2 (length delta))
                         (mapv + (mapv normalize delta) tail)
                         tail)))
                   [0 0])
       set
       count))

(tests
  (part1 (h/parse-input 2022 "9example" "\n")) := 13)

#_(part1 (h/parse-input 2022 9 "\n"))

(defn reapply [n f input]
  #_(nth (iterate f input) n)
  ((apply comp (repeat n f)) input))

(defn part2 [input]
  (->> input
       (map (partial h/rsplit " "))
       (mapcat (fn [[direction repeat-string]]
                 (repeat (parse-long repeat-string) direction)))
       ;; ("R" "L" ...)
       (reductions (fn [memo direction]
                     (mapv + memo (transforms direction)))
                   [0 0])
       (reapply 9 (fn [positions]
                    (reductions (fn [tail head]
                                  (let [delta (mapv - head tail)]
                                    (if (<= 2 (length delta))
                                      (mapv + (mapv normalize delta) tail)
                                      tail)))
                                [0 0]
                                positions)))
       set
       count))

(tests
 (part2 (h/parse-input 2022 "9example" "\n")) := 1
 (part2 (h/parse-input 2022 "9example2" "\n")) := 36)

#_(part2 (h/parse-input 2022 9 "\n"))
