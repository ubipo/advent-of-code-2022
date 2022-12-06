(ns day-03
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [input :refer [load-day-input]]))


(defn load-input [] 
  (let [rucksacks-strings (str/split-lines (load-day-input 3))] 
    (map char-array rucksacks-strings)))

(defn split-in-twine [items]
  (split-at (/ (count items) 2) items))

(defn common-item [compartments]
  (->> compartments
       (map set)
       (apply set/intersection)
       first))

(defn item-priority [item] 
  (let [item-int (int item)]
    ;; Lowercase letters come after upppercase in ASCII/Unicode
    (if (>= item-int (int \a))
     (+ (- item-int (int \a)) 1)
     (+ (- item-int (int \A)) 27))))

(defn part-one []
  (reduce + (map
             #(item-priority (common-item (split-in-twine %)))
             (load-input))))

(defn part-two
  "Total score assuming the second column is the desired round outcome"
  []
  (->> (load-input)
       (partition 3)
       (map #(item-priority (common-item %)))
       (reduce +)))

(defn -main [& _args]
  (println "Part one: " (part-one))
  (println "Part two: " (part-two)))
