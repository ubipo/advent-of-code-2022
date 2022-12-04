(ns day-03
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))


(defn read-input [] 
  (let [rucksacks-strings (str/split-lines (slurp "day_03_input.txt"))] 
    (map char-array rucksacks-strings)))

(defn split-in-twine [items]
  (split-at (/ (count items) 2) items))

(defn common-item [compartments] 
  (first (apply set/intersection (map set compartments))))

(defn item-priority [item] 
  (let [item-int (int item)]
    ;; Lowercase letters come after upppercase in ASCII/Unicode
    (if (>= item-int (int \a))
     (+ (- item-int (int \a)) 1)
     (+ (- item-int (int \A)) 27))))

(defn part-one []
  (reduce + (map
             #(item-priority (common-item (split-in-twine %)))
             (read-input))))

(defn part-two
  "Total score assuming the second column is the desired round outcome"
  []
  (reduce + (map
             #(item-priority (common-item %))
             (partition 3 (read-input)))))

(defn -main [& _args]
  (println "Part one: " (part-one))
  (println "Part two: " (part-two)))
