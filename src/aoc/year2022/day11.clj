(ns aoc.year2022.day11
  (:require
   [aoc.helpers :as h]
   [hyperfiddle.rcf :refer [tests]]
   [clojure.string :as string]))

(defn parse
  [input]
  (->> input
       (h/rsplit "\n\n")
       (map (fn [monkey-string]
              (re-matches #"(?s)Monkey (\d):\n  Starting items: ([0-9, ]+)\n  Operation: new = old (.+?)\n  Test: divisible by (\d+)\n    If true: throw to monkey (\d)\n    If false: throw to monkey (\d)" monkey-string)))
       (map (fn [[_ id starting-items operation divisor true-monkey-target false-monkey-target]]
              (let [[_ string-operator string-value] (re-matches #"(\+|\*) (.*)" operation)]
                {:raw-monkey/id (parse-long id)
                 :raw-monkey/starting-items (map parse-long (string/split starting-items #", "))
                 :raw-monkey/worry-operator (case string-operator
                                              "*" *
                                              "+" +)
                 :raw-monkey/worry-value (if (= "old" string-value)
                                           (fn [old] old)
                                           (fn [_old] (parse-long string-value)))
                 :raw-monkey/divisor (parse-long divisor)
                 :raw-monkey/true-monkey-target (parse-long true-monkey-target)
                 :raw-monkey/false-monkey-target (parse-long false-monkey-target)})))))

(defn finalize-monkeys
  [raw-monkeys post-worry-fn]
  (->> raw-monkeys
       (map (fn [{:raw-monkey/keys [id starting-items worry-operator worry-value divisor true-monkey-target false-monkey-target]}]
              {:monkey/id id
               :monkey/starting-items starting-items
               :monkey/new-worry (fn [old]
                                   (post-worry-fn (worry-operator old (worry-value old))))
               :monkey/next-target (fn [value]
                                     (if (zero? (mod value divisor))
                                       true-monkey-target
                                       false-monkey-target))}))))

(defn process-monkey
  [{:monkey/keys [new-worry next-target]} current-monkey-items]
  (->> current-monkey-items
       (map (fn [item]
              (let [worry (new-worry item)]
                [(next-target worry) worry])))
       (reduce (fn [memo [target worry]]
                 (update memo target (fnil conj []) worry))
               {})))

(tests
 (process-monkey {:monkey/new-worry #(/ % 3)
                  :monkey/next-target inc}
                 [3 6 9])
 := {2 [1] 3 [2] 4 [3]}

 (process-monkey {:monkey/new-worry #(/ % 3)
                  :monkey/next-target (constantly 1)}
                 [3 6 9])
:= {1 [1 2 3]})

(defn round
  [monkeys state]
  (->> monkeys
       (reduce (fn [state monkey]
                 (let [current-monkey-items (get-in state [:state/items (:monkey/id monkey)])]
                   {:state/items (-> (merge-with concat
                                                 (:state/items state)
                                                 (process-monkey monkey current-monkey-items))
                                     (assoc (:monkey/id monkey) []))
                    :state/inspections (-> (:state/inspections state)
                                           (update (:monkey/id monkey) + (count current-monkey-items)))}))
               state)))

(tests
 (round [{:monkey/id 0
          :monkey/new-worry #(/ % 3)
          :monkey/next-target inc}]
        {:state/items {0 [3 6 9]
                       1 [99]
                       2 [99]
                       3 [99]
                       4 []}
         :state/inspections {0 100
                             1 0
                             2 0
                             3 0
                             4 0}})
 := {:state/items {0 []
                   1 [99]
                   2 [99 1]
                   3 [99 2]
                   4 [3]}
     :state/inspections {0 103
                         1 0
                         2 0
                         3 0
                         4 0}})

(defn monkey-business-level
  [state]
  (->> state
       :state/inspections
       (map second)
       sort
       reverse
       (take 2)
       (apply *)))

(defn simulate
  [input round-count make-post-worry-fn]
  (let [raw-monkeys (parse input)
        monkeys (finalize-monkeys raw-monkeys (make-post-worry-fn raw-monkeys))
        state {:state/items (->> monkeys
                                 (map (fn [{:monkey/keys [id starting-items]}]
                                        [id starting-items]))
                                 (into {}))
               :state/inspections (->> monkeys
                                       (map (fn [monkey]
                                              [(:monkey/id monkey) 0]))
                                       (into {}))}]
    (->> (take (inc round-count) (iterate (partial round monkeys) state))
         last
         monkey-business-level)))

(defn part1
  [input]
  (simulate input 20 (fn [_monkeys]
                       (fn [worry]
                         (quot worry 3)))))

(tests
 (part1 (h/get-input 2022 "11example")) := 10605)

#_(part1 (h/get-input 2022 11))

(defn part2
  [input]
  (simulate input 10000 (fn [raw-monkeys]
                          (let [lcm (apply * (map :raw-monkey/divisor raw-monkeys))]
                            (fn [worry]
                              (mod worry lcm))))))

(tests
 (part2 (h/get-input 2022 "11example")) := 2713310158)

#_(part2 (h/get-input 2022 11))
