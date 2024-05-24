(ns aoc.year2016.day4
  (:require
   [clojure.string :as string]
   [hyperfiddle.rcf :as rcf]
   [aoc.helpers :as h]))

(defn parse-line [s]
  (let [[_ name sector checksum] (re-matches #"([a-z-]+)-([0-9]+)\[([a-z]+)\]" s)]
    {:room/name name
     :room/checksum checksum
     :room/sector-id (parse-long sector)}))

(rcf/tests
  (parse-line "aaaaa-bbb-z-y-x-123[abxyz]")
  := {:room/name "aaaaa-bbb-z-y-x"
      :room/checksum "abxyz"
      :room/sector-id 123})

(defn checksum [room]
  (->> (dissoc (frequencies (:room/name room)) \-)
       (sort-by (fn [[k v]] [(- v) k]))
       (take 5)
       (map key)
       (apply str)))

(rcf/tests
  (checksum (parse-line "not-a-real-room-404[oarel]"))
  := "oarel")

(defn real? [room]
  (= (checksum room)
     (:room/checksum room)))

(rcf/tests
  (real? {:room/name "aaaaabbbzyx"
          :room/checksum "abxyz"
          :room/sector-id 123})
  := true)

(defn part-1 [input]
  (->> input
       string/split-lines
       (map parse-line)
       (filter real?)
       (map :room/sector-id)
       (apply +)))

(rcf/tests
  (part-1 (h/get-input 2016 "4sample"))
  := 1514)

#_(part-1 (h/get-input 2016 4)) ;; 245102

(defn shift-char [n c]
  (if (= \- c)
    \space
    (char (+ (int \a)
             (mod (+ (- (int c) (int \a)) n)
                  26)))))

(rcf/tests
  (shift-char 0 \a) := \a
  (shift-char 1 \a) := \b
  (shift-char 26 \a) := \a
  (shift-char 1 \z) := \a)

(defn shift-string [n s]
  (apply str (map (partial shift-char n) s)))

(rcf/tests
  (shift-string 1 "hal") := "ibm")

(defn decrypt [room]
  (shift-string (:room/sector-id room)
                (:room/name room)))

(rcf/tests
  (decrypt (parse-line "qzmt-zixmtkozy-ivhz-343[filler]"))
  := "very encrypted name")

(defn part-2 [input]
  (->> input
       string/split-lines
       (map parse-line)
       (filter real?)
       (filter (fn [room]
                 (string/includes? (decrypt room)
                                   "northpole")))
       first
       :room/sector-id))

#_(part-2 (h/get-input 2016 4)) ;; 324
