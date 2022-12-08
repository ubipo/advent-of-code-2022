(ns day-08
  (:require [clojure.string :as str]
            [input :refer [load-day-input]]))


(defn parse-num-char [char] (- (int char) (int \0)))

(defn load-input []
  (->> (load-day-input 8)
       str/split-lines
       (map #(vec (map parse-num-char (char-array %))))
       vec))

(defn tree-matrix->visibility-matrix [tree-matrix]
  (let [is-on-edge? (fn [x y]
                      (or (= (* x y) 0)
                          (or (= y (dec (count tree-matrix)))
                              (= x (dec (count (first tree-matrix)))))))]
    (->> tree-matrix
         (map-indexed (fn [y row] (vec (map-indexed (fn [x _] (is-on-edge? x y)) row))))
         vec)))

(defn tree-matrix->scenic-matrix [tree-matrix]
  (vec (map (fn [row] (vec (map (fn [_height] 0) row))) tree-matrix)))

(defn reversible-range
  ([end reverse?] (reversible-range 0 end reverse?))
  ([start end reverse?] 
   (if reverse? 
     (reverse (range start end)) 
     (range start end))))

(defn map-matrix-by-direction [matrix [dx dy] line-initial drop-edges? f]
  (assert (or (= (abs dx) 1) (= (abs dy) 1)))
  (assert (not (= (abs dx) (abs dy))))
  (let [xs (reversible-range (count (first matrix)) (neg? dx))
        ys (reversible-range (count matrix) (neg? dy))
        y-major (not= dx 0)]
    (reduce (fn [matrix coord-major]
              (last (reduce (fn [[state matrix] coord-minor] 
                              (let [x (if y-major coord-minor coord-major)
                                    y (if y-major coord-major coord-minor)
                                    value (get-in matrix [y x])
                                    [new-state new-value] (f state [x y] value)]
                              [new-state (assoc-in matrix [y x] new-value)]))
                      [line-initial matrix]
                      ;; Drop last of line because it is always visible (at edge
                      ;; of forest)
                      ;; Don't drop first because it can affect the next trees 
                      (let [minor-coords (if y-major xs ys)]
                        (if drop-edges? (drop-last minor-coords) minor-coords)))))
            matrix
            ;; Drop first and last lines because they are always visible
            (let [major-coords (if y-major ys xs)]
              (if drop-edges? (drop-last (drop 1 major-coords)) major-coords)))))

(defn map-matrix-by-all-directions [matrix line-initial drop-edges? f]
  (->> [[1 0] [-1 0] [0 1] [0 -1]]
       (reduce
        (fn
          [matrix [dx dy]]
          (map-matrix-by-direction matrix [dx dy] line-initial drop-edges? f))
        matrix)))

(defn update-visibilities [tree-matrix visibility-matrix]
  (map-matrix-by-all-directions
   visibility-matrix
   0
   true ;; drop-edges?
   (fn [min-height [x y] was-visible?]
     (let [height (get-in tree-matrix [y x])]
       [(max min-height height) (or was-visible? (> height min-height))])))) ;; drop-edges?

(defn update-scenic-scores [tree-matrix scenic-matrix]
  (map-matrix-by-all-directions
   scenic-matrix
   (vec (repeat 10 0))
   true ;; drop-edges?
   (fn [nbro-visible-trees-by-height [x y] previous-scenic-score]
     (let [height (get-in tree-matrix [y x])
           trees-visible-behind (get nbro-visible-trees-by-height height)
           new-scenic-score (* (max 1 previous-scenic-score) trees-visible-behind)]
       [(vec (apply conj (map inc (drop (inc height) nbro-visible-trees-by-height)) (repeat (inc height) 1)))
        new-scenic-score]))))

(defn part-one [tree-matrix]
  (->> tree-matrix
       tree-matrix->visibility-matrix
       (update-visibilities tree-matrix)
       flatten
       (filter true?)
       count))

(defn part-two [tree-matrix]
  (->> tree-matrix
       tree-matrix->scenic-matrix
       (update-scenic-scores tree-matrix)
       flatten
       (apply max)))

(defn -main [& _args]
  (let [input (load-input)]
    (println "Part one:" (part-one input))
    (println "Part two:" (part-two input))))
