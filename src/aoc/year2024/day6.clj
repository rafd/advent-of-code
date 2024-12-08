(ns aoc.year2024.day6
  (:require
   [clojure.string :as string]
   [hyperfiddle.rcf :as r]
   [aoc.helpers :as h]))

(def ->direction
  {\^ [-1 0]
   \v [1 0]
   \> [0 1]
   \< [0 -1]})

(def rotate
  {\^ \>
   \v \<
   \> \v
   \< \^})

(defn find-guard [grid]
  (->> (for [x (range (count (first grid)))
             y (range (count grid))]
         [y x])
       (some (fn [position]
               (let [guard (get-in grid position)]
                 (when (->direction guard)
                   {:guard guard
                    :guard-position position}))))))

(r/tests
 (find-guard [[\. \>] [\. \.]]) := {:guard \>
                                    :guard-position [0 1]}
 (find-guard [[\. \<]]) := {:guard \<
                            :guard-position [0 1]})

(defn next-state [grid]
  (let [{:keys [guard guard-position]} (find-guard grid)
        next-guard-position (mapv + guard-position (->direction guard))]
    (cond
      ;; no guard
      (nil? guard)
      grid

      ;; would hit obstacle, instead rotate
      (= \# (get-in grid next-guard-position))
      (assoc-in grid guard-position (rotate guard))

      ;; move
      (let [[next-y next-x] next-guard-position]
        (and (<= 0 next-x (dec (count (first grid))))
             (<= 0 next-y (dec (count grid)))))
      (-> grid
          (assoc-in guard-position \X)
          (assoc-in next-guard-position guard))

      ;; going off map
      :else
      (-> grid
          (assoc-in guard-position \X)))))

(r/tests
 (next-state [[\X]]) := [[\X]]
 (next-state [[\^]]) := [[\X]]
 (next-state [[\>]]) := [[\X]]
 (next-state [[\v]]) := [[\X]]
 (next-state [[\<]]) := [[\X]]
 (next-state [[\> \.]]) := [[\X \>]]
 (next-state [[\. \<]]) := [[\< \X]]
 (next-state [[\.] [\^]]) := [[\^] [\X]]
 (next-state [[\v] [\.]]) := [[\X] [\v]]

 (next-state [[\> \#]]) := [[\v \#]])

(defn simulate [grid]
  (loop [grid grid]
    (let [next-grid (next-state grid)]
      (if (= next-grid grid)
        grid
        (recur next-grid)))))

(defn part1 [input]
  (let [grid (->> input
                  string/split-lines
                  (mapv vec))]
    (->> (simulate grid)
         flatten
         (filter #{\X})
         count)))

(r/tests
 (part1 (h/get-input 2024 "6example")) := 41)

;; takes a while
#_(part1 (h/get-input 2024 6)) := 5269
