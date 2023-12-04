(ns aoc.year2023.day4
  (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [hyperfiddle.rcf :as r]
   [aoc.helpers :as h]))

(defn parse-line [line]
  (let [[_ id winning-numbers our-numbers] (re-matches #"Card +(\d+): ([0-9 ]+)\|([0-9 ]+)" line)]
    {:card/id (parse-long id)
     :card/winning-numbers (set (keep parse-long (string/split winning-numbers #" ")))
     :card/our-numbers (set (keep parse-long (string/split our-numbers #" ")))}))

(defn part1 [input]
  (->> input
       (map parse-line)
       (map (fn [{:card/keys [winning-numbers our-numbers]}]
              (if (= 0 (count (set/intersection winning-numbers our-numbers)))
                0
                (Math/pow 2 (dec (count (set/intersection winning-numbers our-numbers)))))))
       (reduce +)
       int))

(r/tests
 (part1 (h/parse-input 2023 "4example" "\n")) := 13)

#_(part1 (h/parse-input 2023 4 "\n")) ;; 25231

(defn won-cards
  [cards card-id]
  (let [{:card/keys [winning-numbers our-numbers]} (get cards (dec card-id))
        matching-count (count (set/intersection winning-numbers our-numbers))]
    (+ 1
       ;; cards are guaranteed to not be out of range
       (reduce + (for [x (range (inc card-id)
                                (+ card-id matching-count 1))]
                   (won-cards cards x))))))

(def won-cards-memo (memoize won-cards))

(defn part2 [input]
  (let [cards (->> input
                   (mapv parse-line))]
    (->> cards
         (map (fn [card]
                (won-cards-memo cards (:card/id card))))
         (reduce +))))

(r/tests
 (part2 (h/parse-input 2023 "4example" "\n")) := 30)

#_(part2 (h/parse-input 2023 4 "\n")) ;; 9721255
