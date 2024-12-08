(ns aoc.year2024.day5
  (:require
   [clojure.string :as string]
   [hyperfiddle.rcf :as r]
   [aoc.helpers :as h]))

(defn part1 [input]
  (let [[raw-rules raw-updates] (string/split input #"\n\n")
        ;; store a map of sets-of-page-pairs to page-to-sort-lower
        ;; ex. {#{123 345} 123}
        ;; this is later used in a custom sort comparator fn
        rules (->> raw-rules
                   string/split-lines
                   ;["123|345" ...]
                   (map (fn [line]
                          (map parse-long (string/split line #"\|"))))
                   (map (fn [pair]
                          [(set pair) (first pair)]))
                   (into {}))
        updates (->> raw-updates
                     string/split-lines
                     (map (fn [line]
                            (map parse-long (string/split line #",")))))
        sorted? (fn [pages]
                  (= pages
                     (sort (fn [a b]
                             (condp  = (rules #{a b})
                               a -1
                               nil 0
                               1))
                           pages)))]
    (->> updates
         (filter sorted?)
         (map (fn [pages]
                (nth pages (quot (count pages) 2))))
         (apply +))))


(r/tests
 (part1 (h/get-input 2024 "5example")) := 143
 (part1 (h/get-input 2024 5)) := 5588)

