
(ns aoc.year2021.day16-2
 (:require
   [aoc.helpers :refer [parse-input]]
   [clojure.test :refer [is deftest testing run-tests]]
   [clojure.walk :as walk]
   [aoc.year2021.day16 :refer [do-the-math]]))

(defn bits->int [bits]
  ;; so dirty
  (BigInteger. (apply str bits) 2))


(def hex->bits
  {\0 '(0 0 0 0)
   \1 '(0 0 0 1)
   \2 '(0 0 1 0)
   \3 '(0 0 1 1)
   \4 '(0 1 0 0)
   \5 '(0 1 0 1)
   \6 '(0 1 1 0)
   \7 '(0 1 1 1)
   \8 '(1 0 0 0)
   \9 '(1 0 0 1)
   \A '(1 0 1 0)
   \B '(1 0 1 1)
   \C '(1 1 0 0)
   \D '(1 1 0 1)
   \E '(1 1 1 0)
   \F '(1 1 1 1)})

(defn count-versions [tree]
  (let [counts (atom 0)]
   (walk/postwalk (fn [x]
                    (if (and (vector? x) (= :version (first x)))
                     (do (swap! counts + (second x))
                       x)
                     x))
                  tree)
   @counts))

(defn do-the-math [tree]
  (walk/postwalk (fn [x]
                   (if (map? x)
                    (case (:type-id x)
                      4 (:value x)
                      0 (apply + (:packets x))
                      1 (apply * (:packets x))
                      2 (apply min (:packets x))
                      3 (apply max (:packets x))
                      5 (if (apply > (:packets x)) 1 0)
                      6 (if (apply < (:packets x)) 1 0)
                      7 (if (apply = (:packets x)) 1 0))
                    x))
                 tree))


#_(->> (clojure.string/split "620080001611562C8802118E34" #"")
       (mapcat hex->bits)
       parse-packets
       clojure.pprint/pprint)

#_(->> (parse-input 2021 "16" "")
       (mapcat hex->bits)
       parse-packets
       count-versions)

#_(->> (parse-input 2021 "16" "")
       (mapcat hex->bits)
       parse-packets
       do-the-math)

;; D2FE28
;; 110100101111111000101000

(defn value-packet? [bits]
  (= 4 (bits->int (take 3 (drop 3 bits)))))

(defn operator-bit-length-packet? [bits]
  (and (not (value-packet? bits))
       (= 0 (nth bits 6))))

(defn operator-packet-count-packet? [bits]
  (and (not (value-packet? bits))
       (= 1 (nth bits 6))))

(defn parse-value-packet [bits]
  (let [value-bits (->> (drop 6 bits)
                        (partition 5)
                        (reduce (fn [memo bits]
                                  (if (= 0 (first bits))
                                    (reduced (conj memo bits))
                                    (conj memo bits)))
                                []))]
    {:version (bits->int (take 3 bits))
     :type-id (bits->int (take 3 (drop 3 bits)))
     :length (+ 6 (* 5 (count value-bits)))
     :value (->> value-bits
                 (mapcat rest)
                 (bits->int))}))

(declare parse-packet)

(defn parse-packets-by-count [count bits]
  (loop [n count
         bits bits
         packets []]
   (if (zero? n)
    packets
    (let [packet (parse-packet bits)]
     (recur (dec n)
            (drop (:length packet) bits)
            (conj packets packet))))))

(defn parse-operator-packet-count-packet [bits]
  (let [header-length (+ 3 3 1 11)
        packets (parse-packets-by-count
                  ;; how many packets to find
                  (bits->int (take 11 (drop 7 bits)))
                  (drop header-length bits))]
   {:version (bits->int (take 3 bits))
    :type-id (bits->int (take 3 (drop 3 bits)))
    :length (apply + header-length (map :length packets))
    :packets packets}))

(defn parse-packets-by-bit-length [bit-length bits]
  (loop [n bit-length
         bits bits
         packets []]
   (if (zero? n)
    packets
    (let [packet (parse-packet bits)]
     (recur (- n (:length packet))
            (drop (:length packet) bits)
            (conj packets packet))))))

(defn parse-operator-bit-length-packet [bits]
  (let [header-length (+ 3 3 1 15)
        packets (parse-packets-by-bit-length
                  (bits->int (take 15 (drop 7 bits)))
                  (drop header-length bits))]
   {:version (bits->int (take 3 bits))
    :type-id (bits->int (take 3 (drop 3 bits)))
    :length (apply + header-length (map :length packets))
    :packets packets}))

(defn parse-packet [bits]
  (cond
    (value-packet? bits)
    (parse-value-packet bits)

    (operator-packet-count-packet? bits)
    (parse-operator-packet-count-packet bits)

    (operator-bit-length-packet? bits)
    (parse-operator-bit-length-packet bits)))


#_(clojure.pprint/pprint (parse-packet (mapcat hex->bits "620080001611562C8802118E34")))


#_(time (->> (parse-input 2021 "16" " ")
             first
             (mapcat hex->bits)
             parse-packet
             do-the-math))

(deftest x
 (testing "parse-packet single value"
   (is (= {:version 6
           :type-id 4
           :length 21
           :value 2021}
          (parse-packet (mapcat hex->bits "D2FE28")))))

 (testing "parse-packets-by-count"
   (is (= [{:version 1
            :type-id 4
            :value 1
            :length 11}
           {:version 2
            :type-id 4
            :value 2
            :length 11}]
          (parse-packets-by-count
            2
            [0 0 1 ;; version
             1 0 0 ;; type-id 4 (value)
             0 0 0 0 1

             0 1 0 ;; version
             1 0 0 ;; type-id 4 (value)
             0 0 0 1 0]))))

 (testing "parse-packets-by-bit-length"
   (is (= [{:version 1
            :type-id 4
            :value 1
            :length 11}
           {:version 2
            :type-id 4
            :value 2
            :length 11}]
          (parse-packets-by-bit-length
            22
            [0 0 1 ;; version
             1 0 0 ;; type-id 4 (value)
             0 0 0 0 1

             0 1 0 ;; version
             1 0 0 ;; type-id 4 (value)
             0 0 0 1 0]))))

 (testing "operator-packet-count-packet?"
   (is (= true
         (operator-packet-count-packet? [0 0 1 ;; version
                                         0 1 0 ;; type-id 2 (minimum)
                                         1 ;; packet-length-id (packet count)
                                         0 0 0 0 0 0 0 0 0 0 1 ;; number of packets
                                           0 0 1 ;; version
                                           1 0 0 ;; type-id 4 (value)
                                           0 0 0 0 1])))))
(testing "parse-packet"
 (is (= {:version 1
         :type-id 2
         :length 29
         :packets [{:version 1
                    :type-id 4
                    :length 11
                    :value 1}]}
        (parse-packet [0 0 1 ;; version
                       0 1 0 ;; type-id 2 (minimum)
                       1 ;; packet-length-id (packet count)
                       0 0 0 0 0 0 0 0 0 0 1 ;; number of packets
                         0 0 1 ;; version
                         1 0 0 ;; type-id 4 (value)
                         0 0 0 0 1])))) ;; 1

(testing "parse-packet"
 (is (= {:version 1
         :type-id 2
         :length 40
         :packets [{:version 1
                    :type-id 4
                    :length 11
                    :value 1}
                   {:version 1
                    :type-id 4
                    :length 11
                    :value 2}]}
        (parse-packet [0 0 1 ;; version
                       0 1 0 ;; type-id 2 (minimum)
                       1 ;; packet-length-id (packet count)
                       0 0 0 0 0 0 0 0 0 1 0 ;; number of packets
                         0 0 1 ;; version
                         1 0 0 ;; type-id 4 (value)
                         0 0 0 0 1

                         0 0 1 ;; version
                         1 0 0 ;; type-id 4 (value)
                         0 0 0 1 0])))) ;; 1

(run-tests)
