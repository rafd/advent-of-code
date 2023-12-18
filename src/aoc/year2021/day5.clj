(ns aoc.year2021.day5
  (:require
    [clojure.string :as string]
    [aoc.helpers :refer [parse-input]]))

(defn string-to-line [string]
  (map parse-long (string/split string #"( -> )|,")))

(defn range-inclusive [start end step]
  (if (and (= start end) (zero? step))
    (repeat start) ;; danger, infinite list!
    (range start (+ end step) step)))

(defn line-to-points [[x1 y1 x2 y2]]
  (map vector (range-inclusive x1 x2 (- (compare x1 x2)))
              (range-inclusive y1 y2 (- (compare y1 y2)))))

#_(->> (parse-input 2021 5 "\n")
       (map string-to-line)
       (mapcat line-to-points)
       frequencies
       (remove (fn [[_ count]]
                 (< count 2)))
       count)
