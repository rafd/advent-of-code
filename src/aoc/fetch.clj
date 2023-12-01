(ns aoc.fetch
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [org.httpkit.client :as http]))

(defn url [year day]
  (str "https://adventofcode.com/" year "/day/" day "/input"))

#_(url 2023 1)

(defn fetch! [year day]
  (let [target-file (io/file (str "resources/inputs/" year "day" day ".txt"))]
    (if (.exists target-file)
      (println year day "already exists")
      (->> @(http/request {:method :get
                           :url (url year day)
                           :user-agent "rafd script"
                           :headers {"Cookie" (str "session=" (string/trim (slurp "AUTH_COOKIE")))}})
           :body
           (spit  target-file)))))

#_(fetch! 2023 1)
