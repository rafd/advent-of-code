(ns aoc.helpers
  (:require
    [clojure.string :as string]
    [clojure.java.io :as io]))

(defn get-input [year day]
 (-> (slurp (io/resource (str year "/day" day ".txt")))
     (string/trim)))

(defn parse-input [year day deliminator]
  (-> (get-input year day)
      (string/split (re-pattern deliminator))))
