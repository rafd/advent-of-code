(ns aoc.year2021.day18
 (:require
   [aoc.helpers :refer [parse-input]]
   [clojure.test :refer [is deftest testing run-tests]]
   [clojure.walk :as walk]))

(defn path-to-number-at-depth [depth number]
  (cond
    (int? number)
    nil
    (= depth 0)
    []
    :else
    (if-let [x (path-to-number-at-depth (dec depth) (first number))]
     (concat [0] x)
     (if-let [x (path-to-number-at-depth (dec depth) (second number))]
      (concat [1] x)))))

(defn path-to-left [path number]
  {:pre [(= 4 (count path))]}
  (if (every? (partial = 0) path)
   nil
   (let [path-prefix (->> path
                           reverse
                           (drop-while zero?)
                           rest
                           reverse)]
      (->> (range 0 (- 5 (count path-prefix)))
           reverse
           (some (fn [x]
                    (let [candidate-path (concat path-prefix [0] (repeat x 1))]
                      (when (get-in number candidate-path)
                       candidate-path))))))))

(defn path-to-right [path number]
  {:pre [(= 4 (count path))]}
  (if (every? (partial = 1) path)
   nil
   (let [path-prefix (->> path
                           reverse
                           (drop-while (partial = 1))
                           rest
                           reverse)]
      (->> (range 0 (- 5 (count path-prefix)))
           reverse
           (some (fn [x]
                    (let [candidate-path (concat path-prefix [1] (repeat x 0))]
                      (when (get-in number candidate-path)
                       candidate-path))))))))

(defn sf-explode [number]
  (let [target-path (path-to-number-at-depth 4 number)
        [l r] (get-in number target-path)
        left-path (path-to-left target-path number)
        right-path (path-to-right target-path number)]
    (cond-> number
            left-path (update-in left-path + l)
            right-path (update-in right-path + r)
            true (assoc-in target-path 0))))

(defn sf-split [number]
  (let [done? (atom false)]
   (walk/postwalk (fn [x]
                    (if (and (int? x) (<= 10 x) (not @done?))
                     (do
                       (reset! done? true)
                       [(int (Math/floor (/ x 2)))
                        (int (Math/ceil (/ x 2)))])
                     x))
                  number)))

(defn can-explode? [number]
  (boolean (path-to-number-at-depth 4 number)))

(defn can-split? [number]
  (boolean (some (partial <= 10) (flatten number))))

(defn sf-reduce [number]
  (cond
    (can-explode? number)
    (recur (sf-explode number))
    (can-split? number)
    (recur (sf-split number))
    :else
    number))

(defn sf-add [x y]
  [x y])

(defn magnitude [number]
 (walk/postwalk (fn [x]
                   (if (vector? x)
                    (+ (* 3 (first x)) (* 2 (second x)))
                    x))
                number))

#_(magnitude [[9,1],[1,9]])
#_(magnitude [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]])

;; part 1

#_(->> (parse-input 2021 "18" "\n")
       ;; it's valid clojure! :)
       (map read-string)
       (reduce (fn [memo number]
                (sf-reduce (sf-add memo number))))
       magnitude)

(defn permutations-2 [list]
 (let [list (vec list)]
  (for [a (range (count list))
        b (range (count list))
        :when (not= a b)]
   [(list a) (list b)])))

;; part 2

#_(->> (parse-input 2021 "18" "\n")
       ;; it's valid clojure! :)
       (map read-string)
       permutations-2
       (map (fn [[a b]]
             (magnitude (sf-reduce (sf-add a b)))))
       (apply max))

(deftest x
  (testing "add"
    (is (= [[1 2] [[3 4] 5]]
           (sf-add [1 2] [[3 4] 5]))))

  (testing "path to number at depth"
    (is (= [0 0 0 0]
           (path-to-number-at-depth 4 [[[[[9,8],1],2],3],4])))
    (is (= [1]
           (path-to-number-at-depth 1 [0,[1,2]])))
    (is (= [1 1 1 1]
           (path-to-number-at-depth 4 [7,[6,[5,[4,[3,2]]]]])))
    (is (= [0 1 1 1]
           (path-to-number-at-depth 4 [[6,[5,[4,[3,2]]]],1]))))

  (testing "path to left of given path"
   (is (= nil
          (path-to-left [0 0 0 0] [[[[[1 2] 3] 4] 5] 6])))
   (is (= [1 1 1 0]
          (path-to-left [1 1 1 1] [1 [2 [3 [4 [5 6]]]]])))
   (is (= [1 1 0]
          (path-to-left [1 1 1 0] [1 [2 [3 [[4 5] 6]]]])))
   (is (= [1 0]
          (path-to-left [1 1 0 0] [1 [2 [[[3 4] 5] 6]]])))
   (is (= [1 0 0]
          (path-to-left [1 0 1 0] [1 [[2 [[3 4] 5]] 6]])))
   (is (= [0 1]
          (path-to-left [1 0 0 0] [[1 2] [[[[3 4] 5] 6] 7]]))))

  (testing "path to left of given path"
    (is (= nil
           (path-to-right [1 1 1 1] [1 [2 [3 [4 [5 6]]]]])))
    (is (= [0 0 0 1]
           (path-to-right [0 0 0 0] [[[[[1 2] 3] 4] 5] 6]))))

  (testing "split"
    (is (= [1, [5 5]]
           (sf-split [1, 10]))))

  (testing "explode"
    (testing "no numbers on the left"
      (is (= [[[[0,9],2],3],4]
             (sf-explode [[[[[9,8],1],2],3],4]))))

    (testing "no numbers on the right"
      (is (= [7,[6,[5,[7,0]]]]
             (sf-explode [7,[6,[5,[4,[3,2]]]]]))))

    (testing "number at address 0 1 1 1"
      (is (= [[6,[5,[7,0]]],3]
             (sf-explode [[6,[5,[4,[3,2]]]],1]))))

    (testing "number at address 0 1 1 0"
      (is (= [[6,[9,[0,5]]],1]
             (sf-explode [[6,[5,[[4,3],2]]],1]))))))

#_(run-tests)
