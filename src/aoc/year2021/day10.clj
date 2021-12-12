(ns aoc.year2021.day10
  (:require
    [aoc.helpers :refer [parse-input]]))

(def bracket-pairings
 {\( \)
  \{ \}
  \[ \]
  \< \>})

(def open-bracket?
  (set (keys bracket-pairings)))

(defn paired? [open close]
  (= close (bracket-pairings open)))

(defn illegal-bracket-or-open-bracket-stack [line]
  (reduce
    (fn [memo bracket]
      (if (open-bracket? bracket)
       (conj memo bracket)
       (let [last-bracket (peek memo)]
         (if (paired? last-bracket bracket)
           (pop memo)
           (reduced bracket)))))
    []
    line))

;; day 10 part 1

(defn first-illegal-character [line]
  (let [result (illegal-bracket-or-open-bracket-stack line)]
    (if (vector? result)
      nil
      result)))

(def illegal-character-points
  {\) 3
   \] 57
   \} 1197
   \> 25137})

#_(->> (parse-input 2021 "10" "\n")
       (map first-illegal-character)
       (remove nil?)
       (map illegal-character-points)
       (apply +))

;; part 2

(def autocomplete-bracket-points
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn autocomplete-score [brackets]
  (reduce
    (fn [score bracket]
      (+ (* score 5) (autocomplete-bracket-points bracket)))
    0
    brackets))

(defn autocomplete [open-bracket-stack]
  (map bracket-pairings (reverse open-bracket-stack)))

(defn median [coll]
  (nth (sort coll) (/ (count coll) 2)))

#_(->> (parse-input 2021 "10" "\n")
       (map illegal-bracket-or-open-bracket-stack)
       (filter vector?)
       (map autocomplete)
       (map autocomplete-score)
       (median))
