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

(defn fetch-instructions! [year day]
  (let [part1-target-file (io/file (str "resources/instructions/" year "day" day "part1.txt"))
        part2-target-file (io/file (str "resources/instructions/" year "day" day "part2.txt"))]
    (if (and (.exists part1-target-file)
             (.exists part2-target-file))
      (println "Instructions for " year day "already exist")
      (e/with-chrome-headless driver
        (doto driver
          (e/go (str "https://adventofcode.com/" year "/day/" day))
          (e/set-cookie {:name "session"
                         :domain "adventofcode.com"
                         :value (string/trim (slurp "AUTH_COOKIE"))})
          (e/go (str "https://adventofcode.com/" year "/day/" day))
          (e/wait-visible {:class "day-desc"}))
        (spit part1-target-file (e/get-element-text driver {:class "day-desc" :index 1}))
        (spit part2-target-file (e/get-element-text driver {:class "day-desc" :index 2}))))))

#_(fetch-instructions! 2023 1)
