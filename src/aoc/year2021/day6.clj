(ns aoc.year2021.day6
  (:require
    [clojure.set :as set]
    [aoc.helpers :refer [parse-input]]))

(defn next-day [age-frequency-map]
  (let [births (get age-frequency-map 0 0)]
    (-> age-frequency-map
        (set/rename-keys {8 7, 7 6, 6 5, 5 4, 4 3, 3 2, 2 1, 1 0, 0 8})
        (update 6 (fnil + 0) births))))

(defn repeatedly-apply [times f]
  (apply comp (repeat times f)))

#_(->> (parse-input 2021 6 ",")
       (map parse-long)
       frequencies
       #_((repeatedly-apply 80 next-day)) ;; part 1
       ((repeatedly-apply 256 next-day)) ;; part 2
       (map second)
       (apply +))
