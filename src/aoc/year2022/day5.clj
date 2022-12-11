(ns aoc.year2022.day5
  (:require
   [aoc.helpers :as h]
   [clojure.string :as string]
   [hyperfiddle.rcf :refer [tests]]
   [com.rpl.specter :as x]))

(defn transpose [m]
  (apply mapv vector m))

(defn pad-nils [x list]
  (concat list (repeat (- x (count list)) nil)))

(tests
 (pad-nils 3 []) := [nil nil nil]
 (pad-nils 3 [1]) := [1 nil nil])

(defn parse-crate-stacks [input]
  (let [stack-count (Integer. ^java.lang.String (second (re-find #"(\d+)\s*$" input)))]
    (->> (string/split input #"\n")
         butlast  ;; skip 1 2 3...
         (map (fn [line]
                (partition-all 3 4 line)))
         (x/transform [x/ALL x/ALL] second)
         (x/transform [x/ALL x/ALL] (fn [x]
                                      (if (= x \space)
                                        nil
                                        x)))
         (map (partial pad-nils stack-count))
         transpose
         (map reverse)
         (map (partial remove nil?))
         (mapv vec))))

(tests
 (parse-crate-stacks "    [D]\n[N] [C]\n[Z] [M] [P]\n 1   2   3")
 := [[\Z \N]
     [\M \C \D]
     [\P]])

(defn crate-mover [input ^long version]
  (let [move-fn (case version
                  9000 reverse
                  9001 identity)
        [crate-stacks-string instructions-string] (string/split input #"\n\n")
        crate-stacks (parse-crate-stacks crate-stacks-string)
        instructions (->> (string/split instructions-string #"\n")
                          (map (fn [line]
                                 (let [[quantity from to] (->> (re-matches #"move (\d+) from (\d+) to (\d+)" line)
                                                                rest
                                                                (map (fn [x] (Integer. ^java.lang.String x))))]
                                   {:quantity quantity
                                    :from from
                                    :to to}))))]
    (->> (reduce (fn [crate-stacks {:keys [quantity from to]}]
                   (-> crate-stacks
                       (update (dec to) into (move-fn (take-last quantity (crate-stacks (dec from)))))
                       (update (dec from) #(vec (drop-last quantity %)))))
            crate-stacks
            instructions)
         (map last)
         (apply str))))

(defn part1 [input]
  (crate-mover input 9000))

(defn part2 [input]
  (crate-mover input 9001))

(tests
 (part1 (h/raw-input 2022 "5example")) := "CMZ"
 (part2 (h/raw-input 2022 "5example")) := "MCD")

#_(part1 (h/raw-input 2022 5))
#_(part2 (h/raw-input 2022 5))
