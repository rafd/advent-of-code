(ns aoc.year2019.day4
  (:require
    [clojure.string :as string]
    [aoc.helpers :as helpers]
    [hyperfiddle.rcf :as rcf]))

(defn has-double-digit? [n]
  (->> (partition 2 1 n)
       (some (partial apply =))
       boolean))

(rcf/tests
  (has-double-digit? [1 2 2 2]) := true
  (has-double-digit? [1 2 3 4]) := false)

(defn increasing? [n]
  (apply <= n))

(rcf/tests
  (increasing? [1 2 3 4]) := true
  (increasing? [1 2 3 3]) := true
  (increasing? [1 2 3 2]) := false)

(defn digits [n]
  (loop [number n
         acc '()]
    (if (zero? number)
      acc
      (recur (quot number 10)
             (conj acc (rem number 10))))))

(rcf/tests
  (digits 1234) := [1 2 3 4])

(defn solve
  [input valid?]
  (->> ;; "123456-234567"
       (string/split input #"-")
       (map parse-long)
       (map + [0 1])
       (apply range)
       ;; [123456 ...]
       (map digits)
       ;; [[1 2 3 4 5 6] ...]
       (filter valid?)
       count))

(defn part-1-valid? [password]
  (and
    (increasing? password)
    (has-double-digit? password)))

(rcf/tests
  (part-1-valid? (digits 111111)) := true
  (part-1-valid? (digits 223450)) := false
  (part-1-valid? (digits 123789)) := false)

(defn part-1
  [input]
  (solve input part-1-valid?))

(rcf/tests
  (part-1 (helpers/get-input 2019 4)) := 945 := 617)

;; part 2

(defn has-exact-double-digit? [n]
  (->> (partition-by identity n)
       (some (fn [x] (= 2 (count x))))
       boolean))

(rcf/tests
  (has-exact-double-digit? [1 2 3]) := false
  (has-exact-double-digit? [1 2 2 2]) := false
  (has-exact-double-digit? [1 2 2 2 3 3]) := true)

(defn part-2-valid?
  [password]
  (and
    (increasing? password)
    (has-exact-double-digit? password)))

(rcf/tests
  (part-2-valid? (digits 112233)) := true
  (part-2-valid? (digits 123444)) := false
  (part-2-valid? (digits 111122)) := true)

(defn part-2
  [input]
  (solve input part-2-valid?))

(rcf/tests
  (part-2 (helpers/get-input 2019 4)) := 617)

