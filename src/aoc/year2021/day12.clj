(ns aoc.year2021.day12
  (:require
    [aoc.helpers :refer [parse-input]]
    [clojure.string :as string]))

(defn find-neighboring-nodes [node connections]
  (->> connections
       (filter (fn [connection] (contains? connection node)))
       (map (fn [connection] (disj connection node)))
       (map first)))

(defn small? [node]
  (java.lang.Character/isLowerCase ^java.lang.Character (first node)))

;; part 1

(defn prune [node connections]
  (remove (fn [connection] (contains? connection node)) connections))

(defn find-paths-1 [current-node target-node path connections]
  (if (= current-node target-node)
    [(conj path current-node)]
    (let [pruned-connections (if (small? current-node)
                               (prune current-node connections)
                               connections)]
      (->> connections
           (find-neighboring-nodes current-node)
           (mapcat (fn [node]
                     (find-paths-1 node target-node (conj path current-node) pruned-connections)))))))

#_(->> (parse-input 2021 "12" "\n")
       #_["start-a" "a-end"]
       #_["start-a" "a-b" "b-d" "a-c" "c-d" "d-end"]
       #_["start-A" "A-b" "b-c" "c-A" "A-end"]
       (map #(string/split % #"-"))
       (map set)
       (find-paths-1 "start" "end" [])
       count)

;; part 2

(defn double-visited? [path]
  (->> path
       (filter small?)
       (frequencies)
       (vals)
       (filter (partial = 2))
       seq
       boolean))

(defn visited? [node path]
  (contains? (set path) node))

(defn find-paths-2 [target-node path connections]
  (if (= (peek path) target-node)
    [path]
    (->> connections
         (find-neighboring-nodes (peek path))
         (remove (fn [node]
                   (or (= node "start")
                       (and (small? node)
                            (visited? node path)
                            (double-visited? path)))))
         (mapcat (fn [node]
                   (find-paths-2 target-node (conj path node) connections))))))

#_(->> (parse-input 2021 "12" "\n")
       #_["start-a" "a-end"]
       #_["start-a" "a-b" "b-d" "a-c" "c-d" "d-end"]
       #_["start-A" "A-b" "b-c" "c-A" "A-end"]
       (map #(string/split % #"-"))
       (map set)
       (find-paths-2 "end" ["start"])
       count)
