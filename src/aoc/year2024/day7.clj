(ns aoc.year2024.day7
  (:require
   [clojure.string :as string]
   [clojure.math.combinatorics :as combo]
   [hyperfiddle.rcf :as r]
   [aoc.helpers :as h]))

(defn parse-line [line]
  (let [[_ result nums] (re-matches #"(\d+): (.*)" line)]
    {:result (parse-long result)
     :nums (map parse-long (string/split nums #" "))}))

(defn calculate [nums operators]
  (if (seq operators)
    (recur
     (conj (drop 2 nums)
           ((first operators) (first nums) (second nums)))
     (rest operators))
    (first nums)))

(r/tests
 (calculate [2 3] [*]) := 6
 (calculate [2 3 1] [* +]) := 7)

(defn possibly-true? [operators {:keys [result nums]}]
  (->> (combo/selections operators (dec (count nums)))
       (some (fn [operators]
               (= result
                  (calculate nums operators))))))

(defn solve [input operators]
  (->> input
       string/split-lines
       (map parse-line)
       (filter (partial possibly-true? operators))
       (map :result)
       (apply +)))

(defn part1 [input]
  (solve input [+ *]))

(r/tests
 (part1 (h/get-input 2024 "7example")) := 3749
 #_(part1 (h/get-input 2024 7)) := 1430271835320)

(defn || [x y]
  (parse-long (str x y)))

(defn part2 [input]
  (solve input [+ * ||]))

 (r/tests
  (part2 (h/get-input 2024 "7example")) := 11387
  #_(part2 (h/get-input 2024 7)) := 456565678667482)


