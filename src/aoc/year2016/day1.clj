(ns aoc.year2016.day1
 (:require
   [clojure.set :as set]
   [aoc.helpers :refer [parse-input]]))

(def heading->unit-vector
  {:N [0 1]
   :E [1 0]
   :S [0 -1]
   :W [-1 0]})

(def turn
  {[:N :L] :W
   [:E :L] :N
   [:S :L] :E
   [:W :L] :S
   [:W :R] :N
   [:N :R] :E
   [:E :R] :S
   [:S :R] :W})

(defn vec+ [va vb]
  (mapv + va vb))

#_(vec+ [1 2 3] [4 5 6])

(defn vec* [mag vb]
  (mapv (partial * mag) vb))

#_(vec* 2 [4 5 6])

(defn distance [[x y]]
  (+ (Math/abs x)
     (Math/abs y)))

;; part 1

(->> (parse-input 2016 1 ", ")
     (map (fn [instruction]
            {:direction (keyword (str (first instruction)))
             :magnitude (Integer. (apply str (rest instruction)))}))
     (reduce (fn [{:keys [heading location]} {:keys [direction magnitude]}]
               (let [new-heading (turn [heading direction])]
                {:heading new-heading
                 :location (vec+ location (vec* magnitude (heading->unit-vector new-heading)))}))
       {:heading :N
        :location [0 0]})
     :location
     distance)

;; part 2

(defn travel [location vector magnitude]
  (rest (take (inc magnitude) (iterate (partial vec+ vector) location))))

#_(travel [0 0] [0 1] 5)

(defn first-already-visited? [visited new]
 (some visited new))

#_(first-already-visited? #{1 2 3} [3 2])
#_(first-already-visited? #{1 2 3} [4])

(->> (parse-input 2016 1 ", ") #_["R8" "R4" "R4" "R8"]
     (map (fn [instruction]
            {:direction (keyword (str (first instruction)))
             :magnitude (Integer. (apply str (rest instruction)))}))
     (reduce (fn [{:keys [heading location visited-locations]} {:keys [direction magnitude]}]
               (let [new-heading (turn [heading direction])
                     new-locations (travel location (heading->unit-vector new-heading) magnitude)
                     new-location (last new-locations)]
                (if-let [repeat-location (first-already-visited? visited-locations new-locations)]
                 (reduced repeat-location)
                 {:heading new-heading
                  :location new-location
                  :visited-locations (into visited-locations new-locations)})))
       {:heading :N
        :location [0 0]
        :visited-locations #{}})
     distance)
