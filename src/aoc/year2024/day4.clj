(ns aoc.year2024.day4
  (:require
   [clojure.string :as string]
   [hyperfiddle.rcf :as r]
   [aoc.helpers :as h]))

(defn horizontal [input]
  (count (re-seq #"XMAS" input)))

(defn reverse-horizontal [input]
  (count (re-seq #"SAMX" input)))

(defn vertical [width input]
  (count (re-seq (re-pattern
                  (str
                   "X(?=.{" width "}"
                   "M.{" width "}"
                   "A.{" width "}"
                   "S)")) input)))

(defn reverse-vertical [width input]
  (count (re-seq (re-pattern
                  (str
                   "S(?=.{" width "}"
                   "A.{" width "}"
                   "M.{" width "}"
                   "X)")) input)))

(defn diagonal-south-east [width input]
  (count (re-seq (re-pattern
                  (str
                   "X(?=.{" (inc width) "}"
                   "M.{" (inc width) "}"
                   "A.{" (inc width) "}"
                   "S)")) input)))

(defn diagonal-north-west [width input]
  (count (re-seq (re-pattern
                  (str
                   "S(?=.{" (inc width) "}"
                   "A.{" (inc width) "}"
                   "M.{" (inc width) "}"
                   "X)")) input)))

(defn diagonal-south-west [width input]
  (count (re-seq (re-pattern
                  (str
                   "X(?=.{" (dec width) "}"
                   "M.{" (dec width) "}"
                   "A.{" (dec width) "}"
                   "S)")) input)))

(defn diagonal-north-east [width input]
  (count (re-seq (re-pattern
                  (str
                   "S(?=.{" (dec width) "}"
                   "A.{" (dec width) "}"
                   "M.{" (dec width) "}"
                   "X)")) input)))

(r/tests
 (let [input (h/get-input 2024 "4example")
       width (.indexOf input "\n")
       input (string/replace input "\n" "*")]
   (horizontal input) := 3
   (reverse-horizontal input) := 2
   (vertical width input) := 1
   (reverse-vertical width input) := 2
   (diagonal-south-east width input) := 1
   (diagonal-north-west width input) := 4
   (diagonal-south-west width input) := 1
   (diagonal-north-east width input) := 4))

(defn part1 [input]
  (let [width (.indexOf input "\n")
        ;; replace \n, because regexes don't work cross-line
        input (string/replace input "\n" "*")]
    (+ (horizontal input)
       (reverse-horizontal input)
       (vertical width input)
       (reverse-vertical width input)
       (diagonal-south-east width input)
       (diagonal-north-west width input)
       (diagonal-south-west width input)
       (diagonal-north-east width input))))

  (r/tests
   (part1 (h/get-input 2024 "4example")) := 18
   (part1 (h/get-input 2024 4)) := 2549)

