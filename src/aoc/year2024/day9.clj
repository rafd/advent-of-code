(ns aoc.year2024.day9
  (:require
   [clojure.string :as string]
   [hyperfiddle.rcf :as r]
   [aoc.helpers :as h]))

(defn defragment [memory]
  (loop [memory (transient memory)
         left-pointer 0
         right-pointer (dec (count memory))]
    (cond
      (= left-pointer right-pointer)
      (persistent! memory)

      (memory left-pointer)
      (recur memory (inc left-pointer) right-pointer)

      (nil? (memory right-pointer))
      (recur memory left-pointer (dec right-pointer))

      :else
      (recur (-> memory
                 (assoc! left-pointer (memory right-pointer))
                 (assoc! right-pointer nil))
             (inc left-pointer)
             (dec right-pointer)))))

(defn checksum [memory]
  (->> memory
       (remove nil?)
       (map-indexed (fn [i id]
                      (* i id)))
       (apply +)))

(defn part1 [input]
  (let [nums (->> (string/split input #"")
                  (map parse-long))
        memory (->> nums
                    (partition 2 2 [0])
                    (map-indexed (fn [i [n gaps]]
                                   (concat (repeat n i) (repeat gaps nil))))
                    (apply concat)
                    vec)]
    (->> memory
         defragment
         checksum)))

(r/tests
 (part1 (h/get-input 2024 "9example")) := 1928)

#_(part1 (h/get-input 2024 9)) := 6370402949053
