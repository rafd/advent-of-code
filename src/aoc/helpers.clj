(ns aoc.helpers
  (:require
    [clojure.string :as string]
    [clojure.java.io :as io]))

(defn parse-input [year day deliminator]
  (-> (slurp (io/resource (str year "/day" day ".txt")))
      (string/trim)
      (string/split (re-pattern deliminator))))
