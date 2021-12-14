(ns aoc.year2021.day12tuned
  (:require
    [aoc.helpers :refer [parse-input]]
    [clojure.string :as string]))

(defn find-paths [node target-node visited-small-set double-visited? connections]
  (->> (connections node)
       (reduce (fn [path-count neighbor-node]
                 (let [small? (java.lang.Character/isLowerCase ^java.lang.Character (first neighbor-node))
                       visited? (contains? visited-small-set neighbor-node)]
                  (cond
                    (= target-node neighbor-node)
                    (+ path-count 1)
                    (and double-visited?
                         small?
                         visited?)
                    path-count
                    :else
                    (+ path-count
                       (find-paths neighbor-node
                                   target-node
                                   (if small?
                                    (conj visited-small-set neighbor-node)
                                    visited-small-set)
                                   (or double-visited?
                                       (and small?
                                            visited?))
                                   connections)))))
               0)))

#_(time (->> (parse-input 2021 12 "\n")
             (map #(string/split % #"-"))
             #_(map #(map keyword %))
             (reduce (fn [memo [node-a node-b]]
                       (-> memo
                           (update node-a (fnil conj #{}) node-b)
                           (update node-b (fnil conj #{}) node-a)))
                     {})
             (map (fn [[a bs]]
                   [a (-> bs (disj "start"))]))
             (into {})
             (find-paths "start" "end" #{"start"} false)))
