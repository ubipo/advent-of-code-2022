(ns day-14
  (:require [clojure.string :as str]
            [input :refer [load-day-input]]))


(defn parse-structure-str [structure-str]
  (map (fn [coords-str] (map parse-long (str/split coords-str #",")))
       (str/split structure-str #" -> ")))

(defn load-input []
  (->> (load-day-input 14)
       str/split-lines
       (map parse-structure-str)))

(def pour-source [500 0])

(defn scan-structures->slice [floor? structures]
  (let [coords (apply concat structures)
        x-coords (map first coords)
        y-coords (map second coords)
        [px py] pour-source
        min-x (apply min px x-coords)
        min-y (apply min py y-coords)
        max-x (apply max px x-coords)
        max-y (+ (apply max py y-coords) (if floor? 1 0))]
    [[[min-x min-y] [max-x max-y]]
     (reduce
      (fn [slice structure]
        (reduce
         (fn [slice [[ax ay] [bx by]]]
           (reduce
            (fn [slice [x y]] (assoc-in slice [(- y min-y) (- x min-x)] \#))
            slice
            (for [x (range (min ax bx) (inc (max ax bx)))
                  y (range (min ay by) (inc (max ay by)))]
              [x y])))
         slice
         (partition 2 1 structure)))
      (into [] (repeat (inc (- max-y min-y))
                       (into [] (repeat (inc (- max-x min-x)) \.))))
      structures)]))

(defn is-free? [floor? slice [[min-x min-y] [_max-x max-y]] [x y]]
  (and
   (or (not floor?) (<= y max-y))
   (= (get-in slice [(- y min-y) (- x min-x)] \.) \.)))

(defn expand-slice [[[min-x min-y] [max-x max-y]] slice [x _y]]
  (let [[[new-min-x new-min-y] [new-max-x new-max-y] :as new-bounds]
        [[(min min-x x) min-y] [(max max-x x) max-y]]
        new-slice (into [] (repeat (inc (- new-max-y new-min-y))
                                   (into [] (repeat (inc (- new-max-x new-min-x)) \.))))
        ;; Add data from old slice back
        new-slice (reduce
                   (fn [new-slice [y x]]
                     (assoc-in new-slice [(- y new-min-y) (- x new-min-x)]
                               (get-in slice [(- y min-y) (- x min-x)])))
                   new-slice
                   (for [x (range min-x (inc max-x))
                         y (range min-y (inc max-y))]
                     [y x]))]
    [new-bounds new-slice]))

(defn drop-sand [floor? bounds slice]
  (let [[px py] pour-source
        is-free? #(is-free? floor? slice bounds %)]
    (if (not (is-free? [px py]))
      nil
      (loop [bounds bounds, slice slice, [sx sy] pour-source]
        (let [[[min-x min-y] [max-x max-y]] bounds
              out-of-x-bounds (or (< sx min-x) (> sx max-x))
              out-of-y-bounds (or (< sy min-y) (> sy max-y))
              [bounds slice] (if (and floor? out-of-x-bounds)
                               (expand-slice bounds slice [sx sy])
                               [bounds slice])
              [[min-x min-y] _] bounds]
          (if (or (and (not floor?) out-of-x-bounds) out-of-y-bounds)
            nil
            (cond
              (is-free? [sx (inc sy)]) (recur bounds slice [sx (inc sy)])
              (is-free? [(dec sx) (inc sy)]) (recur bounds slice [(dec sx) (inc sy)])
              (is-free? [(inc sx) (inc sy)]) (recur bounds slice [(inc sx) (inc sy)])
              :else [bounds (assoc-in slice [(- sy min-y) (- sx min-x)] \o)])))))))

(defn part-one [structures]
  (let [floor? false
        [bounds slice] (scan-structures->slice floor? structures)]
    (loop [bounds bounds, slice slice, count 0]
      (let [[bounds slice] (drop-sand floor? bounds slice)]
        (if (nil? slice)
          count
          (recur bounds slice (inc count)))))))

(defn part-two [structures]
  (let [floor? true
        [bounds slice] (scan-structures->slice floor? structures)]
    (loop [bounds bounds, slice slice, count 0]
      (let [[bounds slice] (drop-sand floor? bounds slice)]
        (if (nil? slice)
          count
          (recur bounds slice (inc count)))))))

(defn -main [& _args]
  (let [input (load-input)]
    (println "Part one:" (part-one input))
    (println "Part two:" (part-two input))))
