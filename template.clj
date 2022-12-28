(ns template
  (:require [clojure.string :as str]
            [input :refer [load-day-input]]))


(defn load-input []
  (->> (load-day-input -1)
       str/split-lines))

(defn part-one [input]
  "part one")

(defn part-two [input]
  "part two")

(defn -main [& _args]
  (let [input (load-input)]
    (println "Part one:" (part-one input))
    (println "Part two:" (part-two input))))
