(ns aoc.year2021.day17
 (:require
   [aoc.helpers :refer [parse-input]]))

(defn velocity-x-lower-bound [[target-x-min _ _ _]]
  (int (Math/ceil (/ (+ -1 (Math/sqrt (+ 1 (* 8 target-x-min))))
                     2))))

(defn velocity-x-upper-bound [[_ target-x-max _ _]]
  target-x-max)

;; TODO can improve
(defn steps-lower-bound [_ _]
  1)

;; TODO can improve
(defn steps-upper-bound [[_ target-x-max _ _ :as target-box] velocity-x]
  (inc (velocity-x-upper-bound target-box))
  #_(Math/ceil (/ (+ -1 (Math/sqrt (+ 1 (* 8 target-x-max))))
                  2)))

;; TODO can improve
(defn velocity-y-lower-bound [[_ _ target-y-min _] _]
  target-y-min)

;; TODO can improve
(defn velocity-y-upper-bound [[_ _ _ target-y-max] steps]
  (* 3 ;; YOLO
    (Math/floor (+ (/ target-y-max steps)
                   (/ (- steps 1)
                      2)))))

(defn hits-target? [[target-x-min target-x-max
                     target-y-min target-y-max]
                    [velocity-x velocity-y]]
  (loop [position-x 0
         position-y 0
         velocity-x velocity-x
         velocity-y velocity-y
         steps 0]
   (cond
     (or (< target-x-max position-x)
         (< position-y target-y-min))
     false
     (and (<= target-x-min position-x target-x-max)
          (<= target-y-min position-y target-y-max))
     steps
     :else
     (recur (+ position-x velocity-x)
            (+ position-y velocity-y)
            (max 0 (dec velocity-x))
            (dec velocity-y)
            (inc steps)))))

#_(hits-target? [20 30 -10 -5] [7 2]) ; true
#_(hits-target? [20 30 -10 -5] [6 3]) ; true
#_(hits-target? [20 30 -10 -5] [9 0]) ; true
#_(hits-target? [20 30 -10 -5] [17 -4]) ; false

(defn velocity-bounded-candidates [target-box]
  (->> (for [velocity-x (range (velocity-x-lower-bound target-box)
                               (inc (velocity-x-upper-bound target-box)))
             steps (range (steps-lower-bound target-box velocity-x)
                          (inc (steps-upper-bound target-box velocity-x)))
             velocity-y (range (velocity-y-lower-bound target-box steps)
                               (inc (velocity-y-upper-bound target-box steps)))]
         [velocity-x velocity-y])
       distinct))

(defn velocity-valid-candidates [target-box]
 (->> (velocity-bounded-candidates target-box)
      (filter (partial hits-target? target-box))))

(defn max-height [velocity-y]
  (int (* 1/2 velocity-y (+ 1 velocity-y))))

(defn optimize-velocity [target-box]
  (->> (velocity-valid-candidates target-box)
       (map second)
       (apply max)
       max-height))

#_(sort (velocity-valid-candidates [20 30 -10 -5]))

#_(count (velocity-valid-candidates [20 30 -10 -5])) ; 112

#_(optimize-velocity [20 30 -10 -5]) ; 45
#_(max-height 45) ; 1035

(defn get-target-box []
 (->> (parse-input 2021 17 ": ")
      second
      ;; "x=81..129, y=-150..-108"
      (re-matches #"x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)")
      rest
      (map #(Integer. %))))

#_(let [target-box (get-target-box) #_[20 30 -10 -5]
        p (set (velocity-bounded-candidates target-box))
        min-x (int (apply min (map first p)))
        max-x (int (apply max (map first p)))
        min-y (int (apply min (map second p)))
        max-y (int (apply max (map second p)))
        pad (fn [s]
              (format "%4s" s))
        s (atom "")]
   (swap! s (fn [o] (apply str o (pad " ") (map pad (range min-x (inc max-x))))))
   (dorun
    (for [y (range min-y (inc max-y))]
     (swap! s (fn [o] (apply str o "\n" (pad y) (for [x (range min-x (inc max-x))]
                                                  (pad (or (hits-target? target-box [x y])
                                                           (if (contains? p [x y])
                                                            "."
                                                            " ")))))))))
   (spit "plot.txt" @s))

;; part 1
#_(optimize-velocity (get-target-box))
;; 11175

;; part 2
#_(count (velocity-valid-candidates (get-target-box)))
; 3540
