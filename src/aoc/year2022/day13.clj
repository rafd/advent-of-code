(ns aoc.year2022.day13
 (:require
  [aoc.helpers :as h]
  [hyperfiddle.rcf :refer [tests]]
  [com.rpl.specter :as x]
  [clojure.edn :as edn]))

(defn order [left right]
  (cond
    (and (int? left) (int? right))
    (case (compare left right)
      -1 :correct
      0 :tie
      1 :incorrect)

    (int? left)
    (recur [left] right)

    (int? right)
    (recur left [right])

    (and (empty? left) (empty? right))
    :tie

    (and (empty? left) (seq right))
    :correct

    (and (seq left) (empty? right))
    :incorrect

    (and (seq left) (seq right))
    (case (order (first left) (first right))
      :correct :correct
      :incorrect :incorrect
      :tie (recur (rest left) (rest right)))))

(tests
 (order 1 2) := :correct
 (order 1 1) := :tie
 (order 2 1) := :incorrect

 (order [1 1] [1 2]) := :correct
 (order [1 1] [1 1]) := :tie
 (order [1 2] [1 1]) := :incorrect

 (order [1] 2) := :correct
 (order 1 [1]) := :tie
 (order 2 [1]) := :incorrect

 (order [1] []) := :incorrect
 (order [] [1]) := :correct)

(defn parse [input]
  (->> input
       (x/transform [] (partial h/rsplit "\n\n"))
       (x/transform [x/ALL] (partial h/rsplit "\n"))
       ;; this feels like cheating
       (x/transform [x/ALL x/ALL] edn/read-string)))

(defn part1 [input]
  (->> input
       parse
       (map (fn [pair] (apply order pair)))
       (map-indexed vector)
       (filter (fn [[_index status]]
                 (= status :correct)))
       (map (fn [[index _status]]
              (inc index)))
       (reduce +)))

(tests
 (part1 (h/get-input 2022 "13example")) := 13)

#_(part1 (h/get-input 2022 13))

(defn sort-fn [a b]
  (case (order a b)
    :correct -1
    :tie 0
    :incorrect 1))

(defn part2 [input]
  (let [divider-packets #{[[2]] [[6]]}]
    (->> input
         parse
         (apply concat divider-packets)
         (sort sort-fn)
         (map-indexed vector)
         (filter (fn [[_index list]]
                   (contains? divider-packets list)))
         (map first)
         (map inc)
         (apply *))))

(tests
 (part2 (h/get-input 2022 "13example")) := 140)

#_(part2 (h/get-input 2022 13))
