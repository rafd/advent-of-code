(ns aoc.helpers
  (:require
    [clojure.string :as string]
    [clojure.java.io :as io]))

(set! *print-namespace-maps* false)

(defn raw-input [year day]
  (slurp (io/resource (str "inputs/" year "day" day ".txt"))))

(defn get-input [year day]
 (string/trim (raw-input year day)))

(defn parse-input [year day deliminator]
  (-> (get-input year day)
      (string/split (re-pattern deliminator))))
