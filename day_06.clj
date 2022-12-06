(ns day-06
  (:require [input :refer [load-day-input]]))


(defn find-start-marker-index [buffer marker-length]
  (->> buffer
       (partition marker-length 1)
       (map #(= (count (set %)) marker-length))
       (take-while false?)
       count
       (+ marker-length)))

(defn part-one [input]
  (find-start-marker-index input 4))

(defn part-two [input]
  (find-start-marker-index input 14))

(defn -main [& _args] 
  (let [input (load-day-input 6)]
    (println "Part one:" (part-one input))
    (println "Part two:" (part-two input))))
