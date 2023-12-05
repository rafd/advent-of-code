(ns aoc.year2023.day5
  (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [hyperfiddle.rcf :as r]
   [aoc.helpers :as h]))

(defn parse-line [line]
  (->> line))

(defn part1 [input]
  (let [seeds (map parse-long (string/split (second (re-matches #"seeds: (.*)" (first input))) #" "))
        maps (->> (rest input)
                  (map (fn [m]
                         (let [[what-to-what & rows] (string/split m #"\n")
                               [_ a b] (re-matches #"([a-z]+)-to-([a-z]+) map:" what-to-what)]
                           {:map/from a
                            :map/to b
                            :map/ranges (->> rows
                                             (map (fn [r]
                                                    (let [[destination source length] (map parse-long (string/split r #" "))]
                                                      {:range/source source
                                                       :range/destination destination
                                                       :range/length length}))))})))
                  doall)
        convert (fn [from to value]
                  (let [map (->> maps
                                 (filter (fn [m]
                                           (= from (:map/from m))))
                                 first)
                        range (->> (:map/ranges map)
                                  (sort-by :range/source)
                                  (take-while (fn [{:range/keys [source]}]
                                                (<= source value)))
                                  last)
                        new-value (cond
                                    (nil? range)
                                    value
                                    (<= value (+ (:range/source range) (:range/length range)))
                                    (+ (:range/destination range) value (- (:range/source range)))
                                    :else
                                    value)]
                    (if (= to (:map/to map))
                      new-value
                      (recur (:map/to map) to new-value))))]
    (->> seeds
         (map (fn [seed]
                  (convert "seed" "location" seed)))
         (apply min))))

(r/tests
 (part1 (h/parse-input 2023 "5example" "\n\n")) := 35)

#_(part1 (h/parse-input 2023 5 "\n\n"))

(defn part2 [input]
  (let [seeds (map parse-long (string/split (second (re-matches #"seeds: (.*)" (first input))) #" "))
        seeds (->> seeds
                   (partition 2)
                   (mapcat (fn [[start cnt]]
                             (range start (+ start cnt)))))
         maps (->> (rest input)
                  (map (fn [m]
                         (let [[what-to-what & rows] (string/split m #"\n")
                               [_ a b] (re-matches #"([a-z]+)-to-([a-z]+) map:" what-to-what)]
                           {:map/from a
                            :map/to b
                            :map/ranges (->> rows
                                             (map (fn [r]
                                                    (let [[destination source length] (map parse-long (string/split r #" "))]
                                                      {:range/source source
                                                       :range/destination destination
                                                       :range/length length}))))})))
                  doall)
        convert (fn [from to value]
                  (let [map (->> maps
                                 (filter (fn [m]
                                           (= from (:map/from m))))
                                 first)
                        range (->> (:map/ranges map)
                                   (sort-by :range/source)
                                   (take-while (fn [{:range/keys [source]}]
                                                 (<= source value)))
                                   last)
                        new-value (cond
                                    (nil? range)
                                    value
                                    (<= value (+ (:range/source range) (:range/length range)))
                                    (+ (:range/destination range) value (- (:range/source range)))
                                    :else
                                    value)]
                    (if (= to (:map/to map))
                      new-value
                      (recur (:map/to map) to new-value))))]
    (->> seeds
         (map (fn [seed]
                (convert "seed" "location" seed)))
         (apply min))))

(r/tests
 (part2 (h/parse-input 2023 "5example" "\n\n")) := 46)

#_(part2 (h/parse-input 2023 5 "\n\n"))
