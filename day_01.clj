(ns day-01
  (:require [clojure.string :as string]
            [split :refer [split-by]]
            [input :refer [load-day-input]]))


(defn load-input [] 
  (map 
   (fn [elf_calories] (map #(Integer/parseInt %) elf_calories)) 
   (split-by empty? (string/split-lines (load-day-input 1)))))

(defn part-one [input] 
  (apply max (map #(reduce + %) input)))

(defn part-two [input]
  (reduce + (take-last 3 (sort (map #(reduce + %) input)))))

(defn -main [& _args]
  (let [input (load-input)]
    (println "Part one:" (part-one input))
    (println "Part two:" (part-two input))))
