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
       (map-indexed (fn [i id]
                      (* i (or id 0))))
       (apply +)))

(defn ->memory [nums]
  (->> nums
       (partition 2 2 [0])
       (map-indexed (fn [i [n gaps]]
                      (concat (repeat n i) (repeat gaps nil))))
       (apply concat)
       vec))

(defn part1 [input]
  (let [nums (->> (string/split input #"")
                  (map parse-long))]
    (->> nums
         ->memory
         defragment
         checksum)))

(r/tests
 (part1 (h/get-input 2024 "9example")) := 1928
 (part1 (h/get-input 2024 9)) := 6370402949053)

(defn ->space-index [nums]
  (->> nums
       (partition 2 2)
       (reduce (fn [memo [file-length space-length]]
                 (-> memo
                     (update space-length (fnil conj (sorted-set)) (+ (:index memo) file-length))
                     (update :index (partial + file-length space-length))))
               {:index 0})
       ((fn [x]
          (dissoc x :index)))))

(r/tests
 (->space-index [1 2 3 4]) := {2 #{1} 4 #{6}})

(defn find-start [memory index]
  (let [value (get memory index)]
    (loop [index index]
      (if (not= (get memory (dec index)) value)
        index
        (recur (dec index))))))

(defn fill! [memory start length value]
  (loop [memory memory
         count 0]
    (if (< count length)
      (recur (assoc! memory (+ start count) value)
             (inc count))
      memory)))

(defn defragment-v2 [memory space-index]
  (loop [memory (transient memory)
         space-index space-index
         right-pointer (dec (count memory))]
    (if-let [value (memory right-pointer)]
      (let [file-start (find-start memory right-pointer)
            file-size (inc (- right-pointer file-start))
            [target-destination target-space-size] (->> space-index
                                                        (keep (fn [[space-size indexes]]
                                                                (when (and (seq indexes)
                                                                           (<= file-size space-size))
                                                                  [(first indexes) space-size])))
                                                        (sort-by first)
                                                        first)]
        (cond
          (= file-start 0)
          (persistent! memory)

          (or (nil? target-destination)
              (< file-start target-destination))
          (recur memory space-index (dec file-start))

          :else
          (recur (-> memory
                     (fill! target-destination file-size value)
                     (fill! file-start file-size nil))
                 (-> space-index
                     (update target-space-size disj target-destination)
                     (update (- target-space-size file-size) (fnil conj (sorted-set)) (+ target-destination file-size)))
                 (dec file-start))))
      (recur memory space-index (dec right-pointer)))))

(defn part2 [input]
  (let [nums (->> (string/split input #"")
                  (map parse-long))
        space-index (->space-index nums)]
    (->> (defragment-v2 (->memory nums) space-index)
         checksum)))

(r/tests
 (part2 (h/get-input 2024 "9example")) := 2858
 (part2 (h/get-input 2024 9)) := 6398096697992)
