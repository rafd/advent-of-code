(ns aoc.year2021.day8
  (:require
    [clojure.set :as set]
    [clojure.string :as string]
    [aoc.helpers :refer [parse-input]]))

;; day 8 part 1

#_(->> (parse-input 2021 8 "\n")
       (map #(string/split % #" \| "))
       (map second)
       (mapcat #(string/split % #" "))
       (filter #(#{2 3 4 7} (count %)))
       count)

;; day 8 part 2

(defn ffilter [f coll]
  (first (filter f coll)))

(defn analyze-row [segment-sets]
  (let [freqs (frequencies (mapcat seq segment-sets))
        a (first (set/difference (ffilter (fn [s] (= 3 (count s))) segment-sets)
                                 (ffilter (fn [s] (= 2 (count s))) segment-sets)))
        e (key (ffilter (fn [[_ count]] (= count 4)) freqs))
        b (key (ffilter (fn [[_ count]] (= count 6)) freqs))
        f (key (ffilter (fn [[_ count]] (= count 9)) freqs))
        c (first (set/difference (ffilter (fn [s] (= 2 (count s))) segment-sets)
                                 #{f}))
        d (first (set/difference (ffilter (fn [s] (= 4 (count s))) segment-sets)
                                 #{b c f}))
        g (first (set/difference #{\a \b \c \d \e \f \g}
                                 #{a b c d e f}))]
    {a \a
     b \b
     c \c
     d \d
     e \e
     f \f
     g \g}))

(def seven-segment-lookup
  {#{\c \f} 1
   #{\a \c \d \e \g} 2
   #{\a \c \d \f \g} 3
   #{\b \c \d \f} 4
   #{\a \b \d \f \g} 5
   #{\a \b \d \e \f \g} 6
   #{\a \c \f} 7
   #{\a \b \c \d \e \f \g} 8
   #{\a \b \c \d \f \g} 9
   #{\a \b \c \e \f \g} 0})

(defn decrypt [decryption-key segment-set]
  (->> segment-set
       (map decryption-key)
       set
       seven-segment-lookup))

(defn decrypt-input-row [input]
 (let [[signal-segment-sets output-segment-sets] (->> (string/split input #" \| ")
                                                      (map (fn [s] (map set (string/split s #" ")))))
       decryption-key (analyze-row signal-segment-sets)
       decrypted-values (map (partial decrypt decryption-key) output-segment-sets)]
   (Integer. (apply str decrypted-values))))

#_(decrypt-input-row "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe")

#_(->> (parse-input 2021 8 "\n")
       (map decrypt-input-row)
       (reduce +))
