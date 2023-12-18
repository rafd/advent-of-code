(ns aoc.year2021.day3
  (:require
    [clojure.string :as string]
    [aoc.helpers :refer [parse-input]]))

;; part 1

#_(->> (parse-input 2021 3 "\n")
       (map (fn [binary-string]
              (map (fn [bit-string] (parse-long bit-string))
                   (string/split binary-string #""))))
       (apply map vector) ;; tranpose
       (map frequencies)
       (map (partial sort-by second))
       (map (partial map first))
       (apply map vector) ;; transpose
       (map (partial apply str))
       (map (fn [x] (Integer/parseInt x 2)))
       (apply *))

;; part 1 approach 2

#_(->> (parse-input 2021 3 "\n")
       (map (fn [binary-string]
              (map (fn [bit-string] (case bit-string
                                      "0" -1
                                      "1" 1))
                   (string/split binary-string #""))))
       (reduce (fn [memo i]
                 (map + memo i)))
       (map (fn [x] (if (pos? x) 1 0)))
       ((fn [x]
         (* (Integer/parseInt (apply str x) 2)
            (Integer/parseInt (apply str (map (fn [i] (if (zero? i) 1 0)) x)) 2)))))

;; part 2

#_(let [find-rating (fn [index comparator bit-strings]
                      (if (= 1 (count bit-strings))
                        (first bit-strings)
                        (let [choosing-function (fn [groups]
                                                   (if (comparator (count (groups \1)) (count (groups \0)))
                                                    [0 (groups \0)]
                                                    [1 (groups \1)]))
                              [first-bit grouped-binary-strings] (->> (group-by #(get % index) bit-strings)
                                                                      choosing-function)]
                          (recur (inc index) comparator grouped-binary-strings))))
        nums (parse-input 2021 3 "\n")
        a (find-rating 0 < nums)
        b (find-rating 0 >= nums)]
    (* (Integer/parseInt a 2)
       (Integer/parseInt b 2)))

;; part 2 approach 2

(defn find-most-common-first-bit-tie-breaker-1 [bit-strings]
  (let [f (->> bit-strings
               (map first)
               frequencies)]
    (if (<= (f \0) (f \1))
      \1
      \0)))

(defn find-least-common-first-bit-tie-breaker-0 [bit-strings]
  (let [f (->> bit-strings
               (map first)
               frequencies)]
    (if (<= (f \0) (f \1))
      \0
      \1)))

#_(let [find-rating (fn find-rating [find-target-bit-fn bit-strings]
                      (if (= 1 (count bit-strings))
                        (first bit-strings)
                        (let [target-bit (find-target-bit-fn bit-strings)
                              filtered-bit-strings (filter (fn [s] (= (first s) target-bit)) bit-strings)]
                          (apply str target-bit (find-rating find-target-bit-fn (map rest filtered-bit-strings))))))
        nums (parse-input 2021 3 "\n")
        a (find-rating find-most-common-first-bit-tie-breaker-1 nums)
        b (find-rating find-least-common-first-bit-tie-breaker-0 nums)]
    (* (Integer/parseInt a 2)
       (Integer/parseInt b 2)))
