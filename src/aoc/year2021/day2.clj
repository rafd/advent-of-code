(ns aoc.year2021.day2
  (:require
    [clojure.string :as string]
    [aoc.helpers :refer [parse-input]]))

;; part 1

#_(->> (parse-input 2021 2 "\n")
       (map (fn [instruction]
             (let [[direction amount] (string/split instruction #" ")]
               [(keyword direction) (Integer. amount)])))
       (reduce (fn [memo [direction amount]]
                (case direction
                  :forward (update memo :horizontal-position + amount)
                  :down (update memo :depth + amount)
                  :up (update memo :depth - amount)))
               {:depth 0
                :horizontal-position 0})
       ((fn [{:keys [depth horizontal-position]}]
         (* depth horizontal-position))))

;; part 2

#_(->> (parse-input 2021 2 "\n")
       (map (fn [instruction]
             (let [[direction amount] (string/split instruction #" ")]
               [(keyword direction) (Integer. amount)])))
       (reduce (fn [memo [direction amount]]
                (case direction
                  :forward (-> memo
                              (update :horizontal-position + amount)
                              (update :depth + (* amount (memo :aim))))
                  :down (update memo :aim + amount)
                  :up (update memo :aim - amount)))
               {:aim 0
                :depth 0
                :horizontal-position 0})
       ((fn [{:keys [depth horizontal-position]}]
         (* depth horizontal-position))))
