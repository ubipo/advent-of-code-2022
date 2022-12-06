(ns day-04
  (:require [clojure.string :as str]
            [input :refer [load-day-input]]))


(defn parse-assignment-str [assignment]
  (->> (str/split assignment #"-")
       (map parse-long)
       (zipmap [:lower :upper])))

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
  (->> input
       (filter #(either-one-fully-contains? (first %) (second %)))
        count))

(defn overlap?
  "Returns true if assignment-a and assignment-b overlap."
  [assignment-a assignment-b]
  (and (<= (:lower assignment-a) (:upper assignment-b))
       (<= (:lower assignment-b) (:upper assignment-a))))

(defn part-two [input]
  (->> input
       (filter #(overlap? (first %) (second %)))
       count))

(defn -main [& _args]
  (let [input (load-input)]
    (println "Part one:" (part-one input))
    (println "Part two:" (part-two input))))
