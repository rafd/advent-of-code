(ns aoc.year2023.day1
  (:require
   [aoc.helpers :as h]
   [clojure.string :as string]
   [hyperfiddle.rcf :refer [tests]]))

(defn process-line [line]
  (let [numbers (->> line
                     (filter #{\1 \2 \3 \4 \5 \6 \7 \8 \9 \0})
                     (map (fn [i] (Integer. (str i)))))]
    (+ (* 10 (first numbers))
       (last numbers))))

(defn part1 [input]
  (->> input
       (map process-line)
       (reduce +)))

#_(part1 (h/parse-input 2023 1 "\n")) ;; 55017

(defn replacement [line]
  (reduce (fn [memo [text replace]]
            (string/replace memo text replace))
          line
          {"one" "o1ne"
           "two" "t2wo"
           "three" "th3ree"
           "four" "fo4ur"
           "five" "fi5ve"
           "six" "si6x"
           "seven" "se7ven"
           "eight" "eig8ht"
           "nine" "ni9ne"}))

(tests
 (replacement "eightwo") := "eig8ht2wo")

(defn part2 [input]
  (->> input
       (map replacement)
       (map process-line)
       (reduce +)))

#_(part2 (h/parse-input 2023 1 "\n")) ;; 53539

(tests
 (part1 (h/parse-input 2023 "1example" "\n"))
 := 142

 (part2 (h/parse-input 2023 "1example2" "\n"))
 := 281)

;;;;;;;; V2

(def str->num
  {"one" 1
   "two" 2
   "three" 3
   "four" 4
   "five" 5
   "six" 6
   "seven" 7
   "eight" 8
   "nine" 9
   "1" 1
   "2" 2
   "3" 3
   "4" 4
   "5" 5
   "6" 6
   "7" 7
   "8" 8
   "9" 9})

(defn vector-starts-with?
  [haystack needle]
  (= needle
     (take (count needle) haystack)))

(defn parse-line [line]
  (loop [line (seq line)
         out []]
    (if (seq line)
      (recur (rest line)
             (if-let [value (some (fn [[match replacement]]
                                    (when (vector-starts-with? line (seq match))
                                      replacement))
                                  str->num)]
               (conj out value)
               out))
      out)))

(tests
 (parse-line "2oneight3") := [2 1 8 3])

(defn part2v2 [input]
  (->> input
       (map parse-line)
       (map (fn [numbers]
              (+ (* 10 (first numbers))
                 (last numbers))))
       (reduce +)))

(tests
 (part2v2 (h/parse-input 2023 "1example2" "\n"))
 := 281)

#_(part2v2 (h/parse-input 2023 1 "\n")) ;; 53539
