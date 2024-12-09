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
                   position))))))

(r/tests
 (find-guard [[\. \>] [\. \.]]) := [0 1]
 (find-guard [[\. \<]]) := [0 1])

(defn next-state [{:state/keys [grid guard-position] :as state}]
  (if (nil? guard-position)
    state

    (let [guard (get-in grid guard-position)
          next-guard-position (mapv + guard-position (->direction guard))]
      (cond
        ;; would hit obstacle, instead rotate
        (= \# (get-in grid next-guard-position))
        (assoc-in state (concat [:state/grid] guard-position) (rotate guard))

        ;; move
        (let [[next-y next-x] next-guard-position]
          (and (<= 0 next-x (dec (count (first grid))))
               (<= 0 next-y (dec (count grid)))))
        (-> state
            (assoc-in (concat [:state/grid] guard-position) \X)
            (assoc-in (concat [:state/grid] next-guard-position) guard)
            (assoc :state/guard-position next-guard-position))

        ;; going off map
        :else
        (-> state
            (assoc-in (concat [:state/grid] guard-position) \X)
            (assoc :state/guard-position nil))))))

(defn grid->state [grid]
  {:state/grid grid
   :state/guard-position (find-guard grid)})

(r/tests
 (next-state (grid->state [[\X]]))
 := (grid->state [[\X]])
 (next-state (grid->state [[\^]]))
 := (grid->state [[\X]])
 (next-state (grid->state [[\>]]))
 := (grid->state [[\X]])
 (next-state (grid->state [[\v]]))
 := (grid->state [[\X]])
 (next-state (grid->state [[\<]]))
 := (grid->state [[\X]])
 (next-state (grid->state [[\> \.]]))
 := (grid->state [[\X \>]])
 (next-state (grid->state [[\. \<]]))
 := (grid->state [[\< \X]])
 (next-state (grid->state [[\.] [\^]]))
 := (grid->state [[\^] [\X]])
 (next-state (grid->state [[\v] [\.]]))
 := (grid->state [[\X] [\v]])

 (next-state (grid->state [[\> \#]])) := (grid->state [[\v \#]]))

(defn simulate [state]
  (loop [state state]
    (let [state' (next-state state)]
      (if (= state state')
        state
        (recur state')))))

(defn parse [input]
  (->> input
       string/split-lines
       (mapv vec)))

(defn part1 [input]
  (->> input
       parse
       grid->state
       simulate
       :state/grid
       flatten
       (filter #{\X})
       count))

(r/tests
 (part1 (h/get-input 2024 "6example")) := 41
 (part1 (h/get-input 2024 6)) := 5269)

(defn find-positions [grid]
  (->> (for [x (range (count (first grid)))
             y (range (count grid))]
         [y x])
       (filter (fn [position]
               (= \X (get-in grid position))))))

(defn update-history [state]
  (let [guard-position (:state/guard-position state)
        guard (get-in state (concat [:state/grid] guard-position))]
    (update-in state [:state/history guard-position] (fnil conj #{}) guard)))

(defn simulate2 [state]
  (loop [state (update-history state)]
    (let [state' (-> state
                     next-state
                     update-history)]
      (cond
        (nil? (:state/guard-position state))
        state
        (= (:state/history state) (:state/history state'))
        (assoc state :state/result :state.result/loop-detected)
        :else
        (recur state')))))

(defn part2 [input]
  (let [initial-state (->> input
                           parse
                           grid->state)
        positions-visited (->> (simulate2 initial-state)
                               :state/grid
                               find-positions
                               set)]
    ;; for every spot visited (except the starting position)
    ;; try it as an obstacle and see if it loops
    (->> (disj positions-visited (find-guard (:state/grid initial-state)))
         (filter (fn [position]
                   (= :state.result/loop-detected
                      (-> initial-state
                          (assoc-in (concat [:state/grid] position) \#)
                          simulate2
                          :state/result))))
         count)))

(r/tests
   (part2 (h/get-input 2024 "6example")) := 6)

;; very slow
#_(part2 (h/get-input 2024 6)) := 1957

