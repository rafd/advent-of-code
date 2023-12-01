(ns aoc.year2023.day1
  (:require
   [aoc.helpers :as h]
   [clojure.string :as string]
   [hyperfiddle.rcf :refer [tests]]))

(defn finalize
  [lines-of-numbers]
  (->> lines-of-numbers
       (map (fn [numbers]
              (+ (* 10 (first numbers))
                 (last numbers))))
       (reduce +)))

(defn extract-integers [line]
  (->> line
       (keep (fn [i] (try
                       (Integer. (str i))
                       (catch Exception _
                         nil))))))

(tests
 (extract-integers "2ab3cd") := [2 3])

(defn part1 [input]
  (->> input
       (map extract-integers)
       finalize))

(tests
 (part1 (h/parse-input 2023 "1example" "\n"))
 := 142)

#_(part1 (h/parse-input 2023 1 "\n")) ;; 55017

(defn pre-process
  [line]
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
 (pre-process "eightwo") := "eig8ht2wo")

(defn part2 [input]
  (->> input
       (map pre-process)
       (map extract-integers)
       finalize))

(tests
 (part2 (h/parse-input 2023 "1example2" "\n"))
 := 281)

#_(part2 (h/parse-input 2023 1 "\n")) ;; 53539

;;;;;;;; V2

(def str->int
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

(defn extract-integers-v2
  [line]
  (loop [line (seq line)
         out []]
    (if (seq line)
      (recur (rest line)
             (if-let [value (some (fn [[match replacement]]
                                    (when (vector-starts-with? line (seq match))
                                      replacement))
                                  str->int)]
               (conj out value)
               out))
      out)))

(tests
 (extract-integers-v2 "2oneight3") := [2 1 8 3])

(defn part2v2 [input]
  (->> input
       (map extract-integers-v2)
       finalize))

(tests
 (part2v2 (h/parse-input 2023 "1example2" "\n"))
 := 281)

#_(part2v2 (h/parse-input 2023 1 "\n")) ;; 53539
