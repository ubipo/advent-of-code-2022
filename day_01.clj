(ns day-01
  (:require [clojure.string :as string])
  (:require [split :refer [split-by]]))


(defn read-input []
  (let [input (slurp "input/day_01.txt")]
    (map
     (fn [elf_calories] (map #(Integer/parseInt %) elf_calories))
     (split-by empty? (string/split-lines input)))))

(defn part-one [input] 
  (apply max (map #(reduce + %) input)))

(defn part-two [input]
  (reduce + (take-last 3 (sort (map #(reduce + %) input)))))

(defn -main [& _args]
  (let [input (read-input)]
    (println "Part one:" (part-one input))
    (println "Part two:" (part-two input))))
