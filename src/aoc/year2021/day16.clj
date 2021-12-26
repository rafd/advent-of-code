(ns aoc.year2021.day16
 (:require
   [aoc.helpers :refer [parse-input]]
   [clojure.walk :as walk]))

#_{:version 1
   :type-id 2
   :length-id 1
   :packets [...]
   :value 7}

;; example 1

;; 110100101111111000101000
;; VVVTTTAAAAABBBBBCCCCC
#_{:version 6
   :type-id 4
   :value 2021}

(defn bits->int [bits]
  ;; so dirty
  (BigInteger. (apply str bits) 2))

(defn take-split [n bits]
  [(take n bits) (drop n bits)])

(defn update-at [coll address & args]
  (if (empty? address)
    (apply (first args) coll (rest args))
    (apply
      update-in coll
                (mapcat (fn [x]
                         [:packets x]) address)
                args)))

#_(update-at {:foo 1} [] dissoc :foo)
#_(update-at {:packets [{} {:a 2 :foo 1}]} [1] dissoc :foo)

(defn get-at [coll address k]
 (if (empty? address)
   (get coll k)
   (get (get-in coll
                (mapcat (fn [x]
                         [:packets x]) address))
        k)))

; [] => []
; [0] => [:packets 0]
; [0 0] => [:packets 0 :packets 0]

(defn parse-packets [bits]
  (loop [output {}
         address []
         current :s/version
         bits bits]
   #_(println current address output (take 5 bits))
   #_(println "----")
   (if (empty? bits)
    output
    (case current
      :s/version
      (let [[version-bits rest-bits] (take-split 3 bits)]
       (recur (-> output
                  (update-at address assoc :version
                             (bits->int version-bits))
                  ((fn [o]
                     (if (get-at o (butlast address) :t/subpacket-packet-length)
                       (update-at o (butlast address) update :t/subpacket-packet-length dec)
                       o))))
              address
              :s/type-id
              rest-bits))
      :s/type-id
      (let [[type-id-bits rest-bits] (take-split 3 bits)
            type-id (bits->int type-id-bits)]
       (recur (-> output
                  (update-at address assoc :type-id type-id))
              address
              (if (= 4 type-id)
                :s/value
                :s/length-id)
              rest-bits))
      :s/value
      (let [[value-bits rest-bits] (take-split 5 bits)]
       (case (first value-bits)
        0
        (recur
          (-> output
              (update-at address assoc :value (bits->int (concat (get-at output address :t/value-buffer)
                                                                 (rest value-bits))))
              (update-at address dissoc :t/value-buffer))
          address
          :s/end
          rest-bits)
        1
        (recur
          (-> output
              (update-at address update :t/value-buffer (fnil concat '()) (rest value-bits)))
          address
          :s/value
          rest-bits)))
      :s/end
      (cond
        (empty? address)
        output
        ;; up
        (= (count bits)
           (get-at output (butlast address) :t/packet-ends-at-bit))
        (recur (-> output
                   (update-at (butlast address) dissoc :t/packet-ends-at-bit))
               (vec (butlast address))
               :s/end
               bits)
        ;; next
        (not (nil? (get-at output (butlast address) :t/packet-ends-at-bit)))
        (recur (-> output)
               (conj (vec (butlast address)) (inc (last address)))
               :s/version
               bits)
        ;; up
        (= 0 (get-at output (butlast address) :t/subpacket-packet-length))
        (recur (-> output
                   (update-at (butlast address) dissoc :t/subpacket-packet-length))
               (vec (butlast address))
               :s/end
               bits)
        ;; next
        (not (nil? (get-at output (butlast address) :t/subpacket-packet-length)))
        (recur (-> output)
               (conj (vec (butlast address)) (inc (last address)))
               :s/version
               bits))
      :s/length-id
      (let [length-id (first bits)]
        (case length-id
          0
          (recur
            output
            address
            :s/subpacket-bit-length
            (rest bits))
          1
          (recur
            output
            address
            :s/subpacket-packet-length
            (rest bits))))
      :s/subpacket-bit-length
      (let [[length-bits rest-bits] (take-split 15 bits)]
       (recur
         (-> output
             (update-at address assoc :packets [])
             (update-at address assoc :t/packet-ends-at-bit (- (count rest-bits)
                                                               (bits->int length-bits))))
         (conj address 0)
         :s/version
         rest-bits))
      :s/subpacket-packet-length
      (let [[length-bits rest-bits] (take-split 11 bits)]
       (recur
         (-> output
             (update-at address assoc :packets [])
             (update-at address assoc :t/subpacket-packet-length (bits->int length-bits)))
         (conj address 0)
         :s/version
         rest-bits))))))

(def hex->bits
  {"0" '(0 0 0 0)
   "1" '(0 0 0 1)
   "2" '(0 0 1 0)
   "3" '(0 0 1 1)
   "4" '(0 1 0 0)
   "5" '(0 1 0 1)
   "6" '(0 1 1 0)
   "7" '(0 1 1 1)
   "8" '(1 0 0 0)
   "9" '(1 0 0 1)
   "A" '(1 0 1 0)
   "B" '(1 0 1 1)
   "C" '(1 1 0 0)
   "D" '(1 1 0 1)
   "E" '(1 1 1 0)
   "F" '(1 1 1 1)})

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
