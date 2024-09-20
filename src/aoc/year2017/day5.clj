(ns aoc.year2017.day5
  (:require
    [clojure.string :as string]
    [hyperfiddle.rcf :as rcf]
    [aoc.helpers :as helpers]))

(defn move-count
  [instructions]
  (loop [instructions instructions
         index 0
         steps 0]
    (if (<= 0 index (dec (count instructions)))
      (recur (update instructions index inc)
             (+ index (nth instructions index))
             (inc steps))
      steps)))

(rcf/tests
  (move-count [0 3 0 1 -3]) := 5)

(defn part-1
  [input]
  (->> input
       (string/split-lines)
       (mapv parse-long)
       move-count))

(rcf/tests
  (part-1 (helpers/get-input 2017 5)) := 342669)

;; part 2

(defn move-count-part-2
  [instructions]
  (loop [instructions instructions
         index 0
         steps 0]
    (if (<= 0 index (dec (count instructions)))
      (let [offset (nth instructions index)]
        (recur (update instructions index
                       (if (< offset 3)
                         inc
                         dec))
               (+ index offset)
               (inc steps)))
      steps)))

(rcf/tests
 (move-count-part-2 [0 3 0 1 -3]) := 10)

(defn part-2
  [input]
  (->> input
       (string/split-lines)
       (mapv parse-long)
       move-count-part-2))

(rcf/tests
  (part-2 (helpers/get-input 2017 5)) := 25136209)



