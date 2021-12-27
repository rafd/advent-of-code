
(ns aoc.year2021.day16-2
 (:require
   [aoc.helpers :refer [parse-input]]
   [clojure.test :refer [is deftest testing run-tests]]
   [clojure.walk :as walk]))

(def VERSION-BIT-LENGTH 3)
(def TYPE-ID-BIT-LENGTH 3)
(def VALUE-CHUNK-BIT-LENGTH 5)
(def OPERATOR-LENGTH-ID-BIT-LENGTH 1)
(def OPERATOR-PACKET-COUNT-BIT-LENGTH 11)
(def OPERATOR-BIT-LENGTH-BIT-LENGTH 15)

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

(defn parse-value-packet [bits]
  (let [value-bits (->> (drop (+ VERSION-BIT-LENGTH TYPE-ID-BIT-LENGTH) bits)
                        (partition VALUE-CHUNK-BIT-LENGTH)
                        (reduce (fn [memo bits]
                                  (if (= 0 (first bits))
                                    (reduced (conj memo bits))
                                    (conj memo bits)))
                                []))]
    {:length (+ VERSION-BIT-LENGTH
                TYPE-ID-BIT-LENGTH
                (* VALUE-CHUNK-BIT-LENGTH
                   (count value-bits)))
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
  (let [header-length (+ VERSION-BIT-LENGTH
                         TYPE-ID-BIT-LENGTH
                         OPERATOR-LENGTH-ID-BIT-LENGTH
                         OPERATOR-PACKET-COUNT-BIT-LENGTH)
        packets (parse-packets-by-count
                  ;; how many packets to find
                  (bits->int (take OPERATOR-PACKET-COUNT-BIT-LENGTH
                                   (drop (+ VERSION-BIT-LENGTH
                                            TYPE-ID-BIT-LENGTH
                                            OPERATOR-LENGTH-ID-BIT-LENGTH)
                                         bits)))
                  (drop header-length bits))]
   {:length (apply + header-length (map :length packets))
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
  (let [header-length (+ VERSION-BIT-LENGTH
                         TYPE-ID-BIT-LENGTH
                         OPERATOR-LENGTH-ID-BIT-LENGTH
                         OPERATOR-BIT-LENGTH-BIT-LENGTH)
        packets (parse-packets-by-bit-length
                  (bits->int (take OPERATOR-BIT-LENGTH-BIT-LENGTH
                                   (drop (+ VERSION-BIT-LENGTH
                                            TYPE-ID-BIT-LENGTH
                                            OPERATOR-LENGTH-ID-BIT-LENGTH)
                                         bits)))
                  (drop header-length bits))]
   {:length (apply + header-length (map :length packets))
    :packets packets}))

(defn parse-packet [bits]
  (let [version (bits->int (take VERSION-BIT-LENGTH bits))
        type-id (bits->int (take TYPE-ID-BIT-LENGTH (drop VERSION-BIT-LENGTH bits)))]
   (into
     {:version version
      :type-id type-id}
     (cond
       (= 4 type-id)
       (parse-value-packet bits)

       (= 1 (nth bits (+ VERSION-BIT-LENGTH TYPE-ID-BIT-LENGTH)))
       (parse-operator-packet-count-packet bits)

       (= 0 (nth bits (+ VERSION-BIT-LENGTH TYPE-ID-BIT-LENGTH)))
       (parse-operator-bit-length-packet bits)))))


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
                          0 0 0 1 0]))))) ;; 1

(run-tests)
