(ns aoc.year2016.day6
  (:require
    [clojure.string :as string]
    [hyperfiddle.rcf :as rcf]
    [aoc.helpers :as h]))

(defn transpose [x]
  (apply map vector x))

(defn most-common [coll]
  (key
   (apply max-key
          val
          (frequencies coll))))

(rcf/tests
  (most-common [\a \b \a])
  := \a
  (most-common "aba")
  := \a)

(defn part-1 [input]
  (->> input
       string/split-lines
       transpose
       (map most-common)
       (apply str)))

(rcf/tests
  (part-1 (h/get-input 2016 "6example"))
  := "easter")

#_(part-1 (h/get-input 2016 6)) ;; usccerug

(defn least-common [coll]
  (key
   (apply min-key
          val
          (frequencies coll))))

(defn part-2 [input]
  (->> input
       string/split-lines
       transpose
       (map least-common)
       (apply str)))

(rcf/tests
  (part-2 (h/get-input 2016 "6example"))
  := "advent")

#_(part-2 (h/get-input 2016 6)) ;; cnvvtafc
