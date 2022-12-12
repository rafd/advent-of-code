(ns aoc.year2022.day7
  (:require
   [aoc.helpers :as h]
   [clojure.walk :as walk]
   [hyperfiddle.rcf :refer [tests]]
   [clojure.string :as string]))

(defn construct-tree [input]
  (->> input
       (reduce (fn [memo instruction]
                 (cond
                   (string/starts-with? instruction "$ cd ..")
                   (update memo :current-dir pop)

                   (string/starts-with? instruction "$ cd")
                   (let [[_ target-dir] (re-matches #"\$ cd (.+)$" instruction)
                         target-dir (keyword "dir" target-dir)]
                     (-> memo
                         (update :current-dir conj target-dir)))

                   (string/starts-with? instruction "$ ls")
                   memo

                   (string/starts-with? instruction "dir")
                   (let [[_ dir] (re-matches #"dir (.+)$" instruction)
                         dir (keyword "dir" dir)]
                     (update-in memo
                                (concat [:tree] (:current-dir memo) [dir])
                                {}))

                   :else ; file
                   (let [[_ size file-name] (re-matches #"(\d+) (.+)$" instruction)
                         file-name (keyword "file" file-name)
                         size (Integer. ^java.lang.String size)]
                     (assoc-in memo (concat [:tree] (:current-dir memo) [file-name]) size))))
               {:current-dir []
                :tree {}})
       :tree))

(defn dir-sizes [tree]
  (let [dirs (atom [])]
    (walk/postwalk
     (fn [node]
       (cond
         (map? node)
         (reduce + (vals node))
         (and (map-entry? node) (= "dir" (namespace (key node))))
         (do
           (swap! dirs conj node)
           node)
         :else
         node))
     tree)
    @dirs))

(tests
 (dir-sizes {:dir/root {:file/a 1}})
 := [[:dir/root 1]]

 (dir-sizes {:dir/root {:dir/x {:file/a 1}}})
 := [[:dir/x 1]
     [:dir/root 1]]

 (dir-sizes {:dir/root {:dir/x {:file/a 1
                                :file/b 1}
                        :file/c 1}})
 := [[:dir/x 2]
     [:dir/root 3]])

(defn part1 [input]
  (->> input
       construct-tree
       dir-sizes
       (filter (fn [[_dir size]]
                 (<= size 100000)))
       (map second)
       (reduce +)))

(tests
 (part1 (h/parse-input 2022 "7example" "\n")) := 95437)

#_(part1 (h/parse-input 2022 7 "\n"))

(defn part2 [input]
  (let [space-total 70000000
        space-needed 30000000
        sizes (->> input
                   construct-tree
                   dir-sizes
                   (map second))
        space-used (apply max sizes)
        space-available (- space-total space-used)
        space-to-delete (- space-needed space-available)]
    (->> sizes
         (filter (fn [size]
                   (<=  space-to-delete size)))
         (apply min))))

#_(part2 (h/parse-input 2022 7 "\n"))
