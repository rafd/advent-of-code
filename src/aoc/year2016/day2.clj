(ns aoc.year2016.day2
 (:require
   [clojure.string :as string]
   [aoc.helpers :as helpers]))

(def layout1
  {\L {1 1, 2 1, 3 2
       4 4, 5 4, 6 5
       7 7, 8 7, 9 8}
   \R {1 2, 2 3, 3 3
       4 5, 5 6, 6 6
       7 8, 8 9, 9 9}
   \U {1 1, 2 2, 3 3
       4 1, 5 2, 6 3
       7 4, 8 5, 9 6}
   \D {1 4, 2 5, 3 6
       4 7, 5 8, 6 9
       7 7, 8 8, 9 9}})

(defn move [position direction]
 (get-in layout1 [direction position]))

(defn apply-line [position directions]
 (reduce move
         position
         directions))

#_(apply-line 5 "ULL")

(defn part1 [input]
  (->> (string/split input #"\n")
       (reductions apply-line 5)
       rest
       (apply str)))

#_(part1 "ULL\nRRDDD\nLURDL\nUUUUD")
#_(part1 (helpers/get-input 2016 2))

(def layout2
  {\L {1 1
       2 2, 3 2, 4 3
       5 5, 6 5, 7 6, 8 7, 9 8
       \A \A, \B \A, \C \B
       \D \D}
   \R {1 1
       2 3, 3 4, 4 4
       5 6, 6 7, 7 8, 8 9, 9 9
       \A \B, \B \C, \C \C
       \D \D}
   \U {1 1
       2 2, 3 1, 4 4
       5 5, 6 2, 7 3, 8 4, 9 9
       \A 6, \B 7, \C 8
       \D \B}
   \D {1 3
       2 6, 3 7, 4 8
       5 5, 6 \A, 7 \B, 8 \C, 9 9
       \A \A, \B \D, \C \C
       \D \D}})

(defn part2 [input]
  (->> (string/split input #"\n")
       (reductions
         (fn [position directions]

           (reduce #(get-in layout2 [%2 %1])
                   position
                   directions))
         5)
       rest
       (apply str)))

#_(part2 (helpers/get-input 2016 2))
