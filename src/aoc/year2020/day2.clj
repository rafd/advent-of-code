(ns aoc.year2020.day2
 (:require
   [clojure.string :as string]
   [aoc.helpers :as helpers]))

(defn parse-line [line]
  (let [[_ param1 param2 char password] (re-matches #"(\d+)-(\d+) (.): (.*)" line)]
    {:param1 (parse-long param1)
     :param2 (parse-long param2)
     :char (first char)
     :password password}))

(defn valid1? [{:keys [param1 param2 char password]}]
  (<= param1
      (->> password
           (filter (partial = char))
           count)
      param2))

(defn part1 [input]
  (->> (string/split input #"\n")
       (map parse-line)
       (filter valid1?)
       count))

#_(part1 (helpers/get-input 2020 2))

(defn valid2? [{:keys [param1 param2 char password]}]
  (let [char1? (= char (get password (dec param1)))
        char2? (= char (get password (dec param2)))]
    (or (and char1? (not char2?))
        (and char2? (not char1?)))))

(defn part2 [input]
  (->> (string/split input #"\n")
       (map parse-line)
       (filter valid2?)
       count))

#_(part2 (helpers/get-input 2020 2))
