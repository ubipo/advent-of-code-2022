(ns day-05
  (:require [clojure.string :as str])
  (:require [split :refer [split-by]]))


(def column-width (count "[X] "))

(defn parse-stacks-line [line]
  (map #(nth % 1) (partition-all column-width line)))

(defn transpose [matrix]
  (apply map list matrix))

(defn parse-stacks-lines [stacks-lines]
  (vec (map (fn [stack] (filter #(not (Character/isWhitespace %)) stack)) 
           (transpose (map parse-stacks-line (pop stacks-lines))))))

(defn parse-moves-lines [moves-lines]
  (map
   #(zipmap
     [:amount :from :to]
     (map parse-long (rest (re-matches #"move (\d+) from (\d+) to (\d+)" %))))
   moves-lines))

(defn read-input []
  (let [[stacks-lines moves-lines]
        (split-by empty? (str/split-lines (slurp "day_05.txt")))]
    {:stacks (parse-stacks-lines stacks-lines)
     :moves (parse-moves-lines moves-lines)}))

(def multi-move true)
(def no-multi-move false)

(defn apply-move [is-multi-move stacks move]
  (let [from-i (dec (:from move)) from (nth stacks from-i)
        to-i (dec (:to move)) to (nth stacks to-i)
        amount (min (:amount move) (count from))
        lifted-stack (take amount from)]
    (assoc stacks 
           from-i (drop amount from)
           to-i (concat
                 (if is-multi-move lifted-stack (reverse lifted-stack))
                 to))))

(defn apply-moves [is-multi-move stacks moves]
  (reduce (partial apply-move is-multi-move) stacks moves))

(defn part-one [input] 
  (str/join
   (map first (apply-moves no-multi-move (:stacks input) (:moves input)))))

(defn part-two [input]
  (str/join
   (map first (apply-moves multi-move (:stacks input) (:moves input)))))

(defn -main [& _args]
  (let [input (read-input)]
    (println "Part one:" (part-one input))
    (println "Part two:" (part-two input))))
