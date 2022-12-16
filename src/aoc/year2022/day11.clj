(ns aoc.year2022.day11
  (:require
   [aoc.helpers :as h]
   [hyperfiddle.rcf :refer [tests]]
   [clojure.string :as string]))

(defn parse [input]
  (->> input
       (h/rsplit "\n\n")
       (map (fn [monkey-string]
              (re-matches #"(?s)Monkey (\d):\n  Starting items: ([0-9, ]+)\n  Operation: new = old (.+?)\n  Test: divisible by (\d+)\n    If true: throw to monkey (\d)\n    If false: throw to monkey (\d)" monkey-string)))
       (map (fn [[_ id starting-items operation divisor true-monkey-target false-monkey-target]]
              (let [[_ string-operator string-value] (re-matches #"(\+|\*) (.*)" operation)]
                {:proto-monkey/id (Integer/parseInt id)
                 :proto-monkey/starting-items (map #(Integer/parseInt %) (string/split starting-items #", "))
                 :proto-monkey/worry-operator (case string-operator
                                                "*" *
                                                "+" +)
                 :proto-monkey/worry-value (if (= "old" string-value)
                                             (fn [old] old)
                                             (fn [_old] (Integer/parseInt string-value)))
                 :proto-monkey/divisor (Integer/parseInt divisor)
                 :proto-monkey/true-monkey-target (Integer/parseInt true-monkey-target)
                 :proto-monkey/false-monkey-target (Integer/parseInt false-monkey-target)})))
       (map (fn [{:proto-monkey/keys [id starting-items worry-operator worry-value divisor true-monkey-target false-monkey-target]}]
              {:monkey/id id
               :monkey/starting-items starting-items
               :monkey/new-worry (fn [old]
                                   (worry-operator old (worry-value old)))
               :monkey/next-target (fn [value]
                                     (if (zero? (mod value divisor))
                                       true-monkey-target
                                       false-monkey-target))}))))

(defn process-monkey
  [{:monkey/keys [new-worry next-target]} monkeys-items]
  (->> monkeys-items
       (map (fn [item]
              (let [worry (quot (new-worry item) 3)]
                [(next-target worry) worry])))
       (reduce (fn [memo [target worry]]
                 (update memo target (fnil conj []) worry))
               {})))

(tests
 (process-monkey {:monkey/new-worry identity
                  :monkey/next-target inc}
                 [3 6 9])
 := {2 [1] 3 [2] 4 [3]}

 (process-monkey {:monkey/new-worry identity
                  :monkey/next-target (constantly 1)}
                 [3 6 9])
:= {1 [1 2 3]})

(defn round [monkeys state]
  (->> monkeys
       (reduce (fn [state monkey]
                 (let [monkey-items (get-in state [:state/items (:monkey/id monkey)])]
                   {:state/items (-> (merge-with concat
                                                 (:state/items state)
                                                 (process-monkey monkey monkey-items))
                                     (assoc (:monkey/id monkey) []))
                    :state/inspections (-> (:state/inspections state)
                                           (update (:monkey/id monkey) + (count monkey-items)))}))
               state)))

(tests
 (round [{:monkey/id 0
          :monkey/new-worry identity
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

(defn monkey-business-level [state]
  (->> state
       :state/inspections
       (map second)
       sort
       reverse
       (take 2)
       (apply *)))

(defn part1 [input]
  (let [monkeys (parse input)
        state {:state/items (->> monkeys
                                 (map (fn [{:monkey/keys [id starting-items]}]
                                        [id starting-items]))
                                 (into {}))
               :state/inspections (->> monkeys
                                       (map (fn [monkey]
                                              [(:monkey/id monkey) 0]))
                                       (into {}))}]
    (->> (take 21 (iterate (partial round monkeys) state))
         last
         monkey-business-level)))

(tests
   (part1 (h/get-input 2022 "11example")) := 10605)

#_(part1 (h/get-input 2022 11))
