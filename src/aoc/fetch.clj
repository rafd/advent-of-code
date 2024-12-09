(ns aoc.fetch
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [org.httpkit.client :as http]
   [etaoin.api :as e]))

(defn fetch-puzzle-input! [year day]
  (let [target-file (io/file (str "resources/inputs/" year "day" day ".txt"))]
    (if (.exists target-file)
      (println "Puzzle input for " year day "already exists")
      (->> @(http/request {:method :get
                           :url (str "https://adventofcode.com/" year "/day/" day "/input")
                           :user-agent "rafd script"
                           :headers {"Cookie" (str "session=" (string/trim (slurp "AUTH_COOKIE")))}})
           :body
           (spit target-file)))))

#_(fetch-puzzle-input! 2023 1)

(defn fetch-instructions! [year day part]
  (let [target-file (io/file (str "resources/instructions/" year "day" day "part" part ".txt"))]
    (if (.exists target-file)
      (println "Instructions for " year day part "already exist")
      (e/with-chrome-headless driver
        (doto driver
          (e/go (str "https://adventofcode.com/" year "/day/" day))
          (e/set-cookie {:name "session"
                         :domain "adventofcode.com"
                         :value (string/trim (slurp "AUTH_COOKIE"))})
          (e/go (str "https://adventofcode.com/" year "/day/" day))
          (e/wait-visible {:class "day-desc"}))
        (spit target-file (e/get-element-text driver {:class "day-desc" :index part}))))))

#_(let [year 2024
        day 7]
    (fetch-instructions! year day 1)
    (fetch-instructions! year day 2))
