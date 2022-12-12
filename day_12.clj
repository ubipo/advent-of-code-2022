(ns day-12
  (:require [clojure.string :as str]
            [input :refer [load-day-input]]
            [clojure.data.priority-map :refer [priority-map]]))


(defn find-char-coords [needle elevations]
  (first
   (keep-indexed
    (fn [y row]
      (let [x (first (keep-indexed (fn [x char] (when (= char needle) x)) row))]
        (when x [x y]))) elevations)))

(defn load-input []
  (->> (load-day-input 12)
       str/split-lines
       (map #(vec (char-array %)))
       vec
       ((fn [elevations]
          [elevations
           (find-char-coords \S elevations)
           (find-char-coords \E elevations)]))))

(defn manhatten [[start-x start-y] [end-x end-y]]
  (+ (abs (- start-x end-x)) (abs (- start-y end-y))))

(def max-climb 1)

(defn reconstruct-path [came-from [cx cy]]
  (loop [path [[cx cy]]]
    (let [[parent _] (get came-from (last path))]
      (if (nil? parent) path (recur (conj path parent))))))

(defn get-elevation [elevations [x y]]
  (let [elev (get-in elevations [y x])]
    (case elev
      nil nil
      \S 0
      \E 25 
      (- (int elev) (int \a)))))

(defn ring-floodfill-deltas
  "Returns a lazy sequence of coordinates for a single manhatten floodfill ring 
   of distance n from the origin.
   e.g. (floodfill-deltas 1) => [[1 0] [0 1] [-1 0] [0 -1]]
        (floodfill-deltas 2) => [[2 0] [1 1] [0 2] [-1 1] [-2 0] [-1 -1] [0 -2] [1 -1]]"
  [n]
  (let [rising-pos (range 0 n)
        falling-pos (range n 0 -1)
        falling-neg (range 0 (- n) -1)
        rising-neg (range (- n) 0)]
    (map vector
         (concat rising-pos falling-pos falling-neg rising-neg)
          (concat falling-pos falling-neg rising-neg rising-pos))))

(defn floodfill-deltas []
  (mapcat ring-floodfill-deltas (iterate inc 1)))

(defn get-neighbors-with-step [elevations step-ok? [x y]]
  (let [current-elevation (get-elevation elevations [x y])]
    (->> (ring-floodfill-deltas 1)
         (map (fn [[dx dy]] [(+ x dx) (+ y dy)]))
         (filter (fn [[nx ny]]
                   (if-let [elev (get-elevation elevations [nx ny])]
                     (step-ok? (- elev current-elevation)) false))))))


(defn a* [get-neighbors h start-coords is-end?]
  (loop [open-set (priority-map start-coords (h start-coords))
         came-from {start-coords [nil 0]}]
    (let [[[cx cy] _] (first open-set)
          [_ gurrent-g-score] (get came-from [cx cy])
          neighbors (get-neighbors [cx cy])]
      (if
       (is-end? [cx cy]) (reconstruct-path came-from [cx cy])
        (if (empty? open-set)
          (throw (ex-info "No path found" {:last-coord [cx cy]}))
          (let
           [[open-set came-from] 
            (reduce
             (fn [[open-set came-from] [nx ny]]
               (let [[_ old-cost] (get came-from [nx ny])
                     new-cost (+ gurrent-g-score 1)]
                 (if (or (nil? old-cost) (< new-cost old-cost))
                   [(assoc open-set [nx ny] (+ new-cost (h [nx ny])))
                    (assoc came-from [nx ny] [[cx cy] new-cost])]
                   [open-set came-from])))
             [(pop open-set) came-from]
             neighbors)]
            (recur open-set came-from)))))))

(defn part-one [[elevations start-coords end-coords]]
  (->> (a*
        (fn [coords] (get-neighbors-with-step elevations #(<= % max-climb) coords))
        #(manhatten end-coords %)
        start-coords
        #(= end-coords %))
       count
       dec)) ;; don't count the start coord

(defn closest-with-elevation [elevations elevation [x y]]
  (->> (floodfill-deltas) 
       (map (fn [[dx dy]] [(+ x dx) (+ y dy)]))
       ;; Elevations outside the map are nil, so they get filtered out 
       ;; implicitly
       (filter #(= (get-elevation elevations %) elevation))
       first))

(defn part-two [[elevations _ start-coords]]
  (->> (a*
        (fn [coords] (get-neighbors-with-step elevations #(>= % (- max-climb)) coords))
        (fn [coords]
          (let [closest-low-elevation (closest-with-elevation elevations 0 coords)]
            (manhatten coords closest-low-elevation)))
        start-coords
        #(= (get-elevation elevations %) 0))
       count
       dec)) ;; don't count the start coord

(defn -main [& _args]
  (let [input (load-input)]
    (println "Part one:" (part-one input))
    (println "Part two:" (part-two input))))
