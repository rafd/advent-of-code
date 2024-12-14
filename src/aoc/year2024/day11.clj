(ns aoc.year2024.day11
  (:require
   [clojure.string :as string]
   [hyperfiddle.rcf :as r]
   [aoc.helpers :as h]))

;; naive solution
;; (list of stones => blink => new list of stones => ...)

(defn halve-string [s]
  (let [half-length (quot (count s) 2)]
    [(subs s 0 half-length)
     (subs s half-length)]))

(defn blink [stones]
  (->> stones
       (mapcat (fn [stone]
                 (cond
                   (= 0 stone)
                   [1]
                   (even? (count (str stone)))
                   (map parse-long (halve-string (str stone)))
                   :else
                   [(* stone 2024)])))))

(defn solve [input n]
  (let [stones (->> (string/split input #" ")
                    (map parse-long))]
    (count (reduce (fn [acc _]
                     (blink acc))
                   stones
                   (range 0 n)))))

#_(time (solve (h/get-input 2024 "11example") 25)) ;; 52ms
#_(time (solve (h/get-input 2024 "11example") 26)) ;; 73ms
#_(time (solve (h/get-input 2024 "11example") 27)) ;; 104ms
#_(time (solve (h/get-input 2024 "11example") 28)) ;; 150ms

#_(time (solve (h/get-input 2024 "11") 25)) ;; 150ms

;; takes ??? for 75 blinks
;; it is O(2^N)
;; assuming each step is appx 1.4x the previous, 25=>75 steps is 52ms*1.4^50 = 17500 hours = 731 years (!!!)

;; let's reframe the problem
;; for a single stone, how stones will it create after N blinks?
;; there will be a lot of re-calculations for the same stone + N blink count
;; so... dynamic programming!

(declare memo-stone-count)

(defn stone-count
  [blink-count stone]
  (if (= blink-count 1)
    (if (even? (count (str stone)))
      2
      1)
    (cond
      (= 0 stone)
      (memo-stone-count (dec blink-count) 1)
      (even? (count (str stone)))
      (let [[a b] (halve-string (str stone))]
        (+ (memo-stone-count (dec blink-count) (parse-long a))
           (memo-stone-count (dec blink-count) (parse-long b))))
      :else
      (memo-stone-count (dec blink-count) (* stone 2024)))))

(def memo-stone-count (memoize stone-count))

(defn solve-fast [input n]
  (let [stones (->> (string/split input #" ")
                    (map parse-long))]
    (apply + (map (partial stone-count n) stones))))

#_(time (solve-fast (h/get-input 2024 "11example") 25)) ;; 8ms
#_(time (solve-fast (h/get-input 2024 "11") 25)) ;; 8ms

#_(time (solve-fast (h/get-input 2024 "11example") 75)) ;; 8ms
#_(time (solve-fast (h/get-input 2024 "11") 75)) ;; 130ms

(defn part1 [input]
  (solve-fast input 25))

(defn part2 [input]
  (solve-fast input 75))

(r/tests
 (part1 (h/get-input 2024 "11example")) := 55312
 (part1 (h/get-input 2024 11)) := 217812
 (part2 (h/get-input 2024 "11example")) := 65601038650482
 (part2 (h/get-input 2024 11)) := 259112729857522)

