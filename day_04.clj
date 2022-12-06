(ns day-04
  (:require [clojure.string :as str]
            [input :refer [load-day-input]]))


(defn parse-assignment-str [assignment]
  (zipmap [:lower :upper] (map parse-long (str/split assignment #"-"))))

(defn parse-assignment-pair-str [assignment-pair-str]
  (map parse-assignment-str (str/split assignment-pair-str #",")))

(defn load-input []
  (let [assignment-pairs (str/split-lines (load-day-input 4))]
    (map parse-assignment-pair-str assignment-pairs)))

(defn fully-contains?
  "Returns true if assignment-a fully contains assignment-b"
  [assignment-a assignment-b]
  (and (<= (:lower assignment-a) (:lower assignment-b))
       (<= (:upper assignment-b) (:upper assignment-a))))

(defn either-one-fully-contains?
  "Returns true if assignment-a fully contains assignment-b or vice versa."
  [assignment-a assignment-b]
  (or (fully-contains? assignment-a assignment-b)
      (fully-contains? assignment-b assignment-a)))

(defn part-one [input]
  (count (filter 
          #(either-one-fully-contains? (first %) (second %)) 
          input)))

(defn overlap?
  "Returns true if assignment-a and assignment-b overlap."
  [assignment-a assignment-b]
  (and (<= (:lower assignment-a) (:upper assignment-b))
       (<= (:lower assignment-b) (:upper assignment-a))))

(defn part-two [input]
  (count (filter
          #(overlap? (first %) (second %))
          input)))

(defn -main [& _args]
  (let [input (load-input)]
    (println "Part one:" (part-one input))
    (println "Part two:" (part-two input))))
