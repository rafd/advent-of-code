(ns aoc.year2019.day2
 (:require
   [clojure.string :as string]
   [aoc.helpers :as helpers]))

(defn parse [input]
 (->> (string/split input #",")
      (mapv parse-long)))

(def opcode->fn
  {1 +
   2 *})

(defn run [initial-ram]
  (loop [index 0
         ram initial-ram]
   (let [opcode (ram index)]
    (if (= 99 opcode)
      ram
      (let [a (ram (ram (+ index 1)))
            b (ram (ram (+ index 2)))
            target (ram (+ index 3))]
        (recur (+ index 4)
               (assoc ram target ((opcode->fn opcode) a b))))))))

#_(run (parse "1,0,0,0,99"))
#_(run (parse "1,1,1,4,99,5,6,0,99"))
#_(run (parse "2,4,4,5,99,0"))

(defn part1 [input]
  (-> (parse input)
      (assoc 1 12)
      (assoc 2 2)
      run
      first))

#_(part1 (helpers/get-input 2019 2))

(defn part2 [input]
  (let [initial-ram (parse input)]
   (->> (for [noun (range 0 100)
              verb (range 0 100)]
         [noun verb
          (-> initial-ram
              (assoc 1 noun)
              (assoc 2 verb)
              run
              first)])
        (filter (fn [[n v result]] (= result 19690720)))
        first
        ((fn [[n v _]] (+ (* 100 n) v))))))

#_(part2 (helpers/get-input 2019 2))
