(ns day-05
  (:require [clojure.string :as str]
            [input :refer [load-day-input]]
            [split :refer [split-by]]))


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
     (->> (re-matches #"move (\d+) from (\d+) to (\d+)" %)
          rest
          (map parse-long)))
   moves-lines))

(defn load-input []
  (let [[stacks-lines moves-lines]
        (split-by empty? (str/split-lines (load-day-input 5)))]
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

(defn stack-heads-str [is-multi-move stacks moves]
  (->> moves
       (apply-moves is-multi-move stacks)
       (map first)
       (str/join)))

(defn part-one [input]
  (stack-heads-str no-multi-move (:stacks input) (:moves input)))

(defn part-two [input]
  (stack-heads-str multi-move (:stacks input) (:moves input)))

(defn -main [& _args]
  (let [input (load-input)]
    (println "Part one:" (part-one input))
    (println "Part two:" (part-two input))))
