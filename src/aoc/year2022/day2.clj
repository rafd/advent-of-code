(ns aoc.year2022.day2
  (:require
   [clojure.string :as string]
   [aoc.helpers :as h]
   [hyperfiddle.rcf :refer [tests]]
   [com.rpl.specter :as x]))

(def beats
  {:play/scissors :play/rock
   :play/paper :play/scissors
   :play/rock :play/paper})

(defn result
  [[them us]]
  (cond
    (= them us)
    :result/tie
    (= us (beats them))
    :result/win
    :else
    :result/loss))

(def result-points
  {:result/tie 3
   :result/win 6
   :result/loss 0})

(def play-points
  {:play/rock 1
   :play/paper 2
   :play/scissors 3})

(defn score [[them us]]
  (+ (result-points (result [them us]))
     (play-points us)))

(defn part1 [input]
  (->> input
       (x/transform [x/ALL] #(string/split % #" "))
       (x/transform [x/ALL x/ALL] {"A" :play/rock
                                   "B" :play/paper
                                   "C" :play/scissors
                                   "X" :play/rock
                                   "Y" :play/paper
                                   "Z" :play/scissors})
       (x/transform [x/ALL] score)
       (reduce +)))

(tests
 (part1 (h/parse-input 2022 "2example" "\n")) := 15)

#_(part1 (h/parse-input 2022 2 "\n"))

(defn part2 [input]
  (->> input
       (x/transform [x/ALL] #(string/split % #" "))
       (x/transform [x/ALL x/ALL] {"A" :play/rock
                                   "B" :play/paper
                                   "C" :play/scissors
                                   "X" :result/loss
                                   "Y" :result/tie
                                   "Z" :result/win})
       (x/transform [x/ALL] (fn [[them result]]
                              [them (case result
                                      :result/tie
                                      them
                                      :result/win
                                      (beats them)
                                      :result/loss
                                      (beats (beats them)))]))
       (x/transform [x/ALL] score)
       (reduce +)))

(tests
 (part2 (h/parse-input 2022 "2example" "\n")) := 12)

#_(part2 (h/parse-input 2022 2 "\n"))
