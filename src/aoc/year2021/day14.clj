(ns aoc.year2021.day14
  (:require
    [aoc.helpers :refer [parse-input]]
    [clojure.string :as string]))

;; part 1

(defn step [rules template]
  (->> template
       (partition 2 1 [nil])
       (mapcat (fn [pair]
                 (if-let [to-insert (rules pair)]
                  [(first pair) to-insert]
                  [(first pair)])))))

#_(let [[template rules] (parse-input 2021 "14" "\n\n")
        template (vec template)
        rules (->> (string/split rules #"\n")
                   (map (fn [rule-string]
                          (let [[pair element] (string/split rule-string #" -> ")]
                           [(vec pair) (first element)])))
                   (into {}))
        n-step (apply comp (repeat 40 (partial step rules)))]
    (->> (n-step template)
         frequencies
         vals
         ((fn [counts]
            (- (apply max counts)
               (apply min counts))))))

;; part 2
;; (not enough RAM in the 'verse to do it naively like above)

#_[\N \N \C \B]

#_{[:start \N] 1
   [\N \N] 5
   [\N \C] 1
   [\C \B] 1
   [\B :end] 1}

#_[[\N \N] \C]

#_{[:start \N] 1
   [\N \N] 0 ;; -1
   [\N \C] 2 ;; +1
   [\C \N] 1 ;; +1
   [\C \B] 1
   [\B :end] 1}

(defn step2 [rules template]
  (reduce (fn [memo [[a b] count]]
            (if-let [to-insert (rules [a b])]
              (-> memo
                  (update [a b] - count)
                  (update [a to-insert] (fnil + 0) count)
                  (update [to-insert b] (fnil + 0) count))
              memo))
    template
    template))

#_(step2 {[\N \N] \C} {[nil \N] 1
                       [\N \N] 1
                       [\N \C] 1
                       [\C \B] 1
                       [\B nil] 1})

(defn get-frequencies [template]
  (-> (reduce (fn [memo [[a b] count]]
                (-> memo
                    (update a (fnil + 0) (/ count 2))
                    (update b (fnil + 0) (/ count 2))))
            {}
            template)
      (dissoc nil)))

#_(get-frequencies {[nil \N] 1
                    [\N \N] 1
                    [\N \C] 1
                    [\C \B] 1
                    [\B nil] 1})

#_(let [[template rules] (parse-input 2021 "14" "\n\n")
        template (->> (concat [nil] (vec template) [nil])
                      (partition 2 1)
                      frequencies)
        rules (->> (string/split rules #"\n")
                   (map (fn [rule-string]
                          (let [[pair element] (string/split rule-string #" -> ")]
                           [(vec pair) (first element)])))
                   (into {}))
        n-step (apply comp (repeat 40 (partial step2 rules)))]
    (->> (n-step template)
         get-frequencies
         vals
         ((fn [counts]
            (- (apply max counts)
               (apply min counts))))))
