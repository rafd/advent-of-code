(ns aoc.year2021.day1
  (:require
    [clojure.string :as string]
    [aoc.helpers :refer [parse-input]]))

;; part 1 approach 1

(->> (parse-input 2021 1 "\n")
     (map parse-long)
     (reduce (fn [memo item]
              {:count (+ (memo :count)
                         (if (and (memo :prev-item)
                                  (> item (memo :prev-item)))
                             1
                             0))
               :prev-item item})
             {:count 0
              :prev-item nil}))
;; part 1 approach 2

#_(let [numbers (->> (parse-input 2021 1 "\n")
                     (map parse-long))]
    (count (filter true? (map < numbers (rest numbers)))))

;; part 2

#_(->> (parse-input 2021 1 "\n")
       (map parse-long)
       (partition 3 1)
       (map (partial apply +))
       (partition 2 1)
       (map (partial apply <))
       (filter true?)
       count)
