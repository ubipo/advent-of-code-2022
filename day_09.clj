(ns day-09
  (:require [clojure.string :as str]
            [input :refer [load-day-input]]))


(defn parse-num-char [char] (- (int char) (int \0)))

(defn load-input []
  (->> (load-day-input 9)
       str/split-lines
       (map #(let [[direction distance-str] (str/split % #"\s+" 2)]
               [(first (char-array direction)) (parse-long distance-str)]))))

(defn clamp [num lower upper] (max lower (min upper num)))

(defn follow-next-knot [[kx ky] [nx ny]] ;; knot, next 
  (let [[dx dy] (map - [nx ny] [kx ky])]
    (if (and (<= (abs dx) 1) (<= (abs dy) 1))
      [kx ky] ;; Close enough to next; don't move
      (let [new-tx (+ kx (clamp dx -1 1))
            new-ty (+ ky (clamp dy -1 1))]
        [new-tx new-ty]))))

(defn follow-direction [[hx hy] direction]
  (case direction
    \R [(inc hx) hy]
    \U [hx (inc hy)]
    \L [(dec hx) hy]
    \D [hx (dec hy)]
    (throw (ex-info "Invalid direction" {:direction direction}))))

(defn expand-moves [moves]
  (->> moves
       (map (fn [[direction distance]]
              (repeat distance direction)))
       (apply concat)))

(defn part-one [moves]
  (as-> moves $
    (expand-moves $)
    (reduce
     (fn [[head tail visited-tail-positions] direction]
       (let [new-head (follow-direction head direction)
             new-tail (follow-next-knot tail new-head)]
         [new-head new-tail (conj visited-tail-positions new-tail)]))
     [[0 0] [0 0] #{}] $)
    (let [[_head _tail visited-tail-positions] $]
      visited-tail-positions)
    (count $)))

(defn follow-next-knots [tail-knots next]
  (drop 1
   (reduce
    (fn [new-tail-knots knot]
      (conj new-tail-knots (follow-next-knot knot (last new-tail-knots))))
    [next]
    tail-knots)))

(defn part-two [moves]
  (as-> moves $
    (expand-moves $)
    (reduce
     ;; Tail knots are ordered closest-to-head to farthest-from-head
     (fn [[head tail-knots visited-tail-positions] direction]
       (let [new-head (follow-direction head direction)
             ;; Each tail knot follows the next knot in the list
             new-tail-knots (follow-next-knots tail-knots new-head)]
         [new-head new-tail-knots (conj visited-tail-positions (last new-tail-knots))]))
     [[0 0] (repeat 9 [0 0]) #{}]
     $)
    (let [[_head _tail visited-tail-positions] $]
      visited-tail-positions)
    (count $)))

(defn -main [& _args]
  (let [input (load-input)]
    (println "Part one:" (part-one input))
    (println "Part two:" (part-two input))))
