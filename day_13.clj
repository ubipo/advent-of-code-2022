(ns day-13
  (:require [clojure.string :as str]
            [input :refer [load-day-input]]
            [split :refer [split-by]]))


(defn skip-elem-comma [s]
  (assert (= (first s) \,) "List elements must be separated by commas")
  (subs s 1))

(defn parse-packet-partial [s]
  (case (first s)
    \[ (loop [s (subs s 1)
              elems []
              is-first? true]
         (if (= (first s) \])
           [elems (subs s 1)]
           (let [elem-s (if is-first? s (skip-elem-comma s))
                 [elem remaining] (parse-packet-partial elem-s)]
             (recur remaining (conj elems elem) false))))
    (let [[n-str _] (re-find #"^(\d+)" s)]
      [(parse-long n-str) (subs s (count n-str))])))

(defn parse-packet [s]
  (let [[parsed remaining] (parse-packet-partial s)]
    (assert (empty? remaining) "Packet must be fully parsed")
    parsed))

(defn lists-order [is-right-order left right]
  (loop [left left
         right right]
    (cond
      (and (empty? left) (empty? right)) nil
      (empty? left) :right-order
      (empty? right) :wrong-order
      :else (let [first-order (is-right-order (first left) (first right))]
              (if (nil? first-order)
                (recur (rest left) (rest right))
                first-order)))))

(defn wrap-int [n] (if (int? n) [n] n))

(defn packets-order [left right]
  (cond
    (and (int? left) (int? right)) (if (< left right)
                                     :right-order
                                     (when (> left right) :wrong-order))
    (and (sequential? left) (sequential? right)) (lists-order packets-order left right)
    :else (packets-order (wrap-int left) (wrap-int right))))

(defn right-order? [left right] (= (packets-order left right) :right-order))

(defn load-input []
  (->> (load-day-input 13)
       str/split-lines
       (split-by empty?)
       ((fn [pair] (map #(map parse-packet %) pair)))))

(defn part-one [packet-pairs]
  (->> packet-pairs
       (keep-indexed (fn [i [left right]] (when (right-order? left right) i)))
       (map #(inc %))
       (apply +)))

(defn part-two [packet-pairs]
  (->> packet-pairs 
       (apply concat)
       (#(conj % [[2]] [[6]]))
       (sort #(case (packets-order %1 %2)
                :right-order -1
                :wrong-order 1
                0))
       (map vec)
       (keep-indexed (fn [i packet] (when (or (= packet [[2]]) (= packet [[6]])) i)))
       (map inc)
       (apply *)))

(defn -main [& _args]
  (let [input (load-input)]
    (println "Part one:" (part-one input))
    (println "Part two:" (part-two input))))
