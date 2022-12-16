(ns day-15
  (:require [clojure.string :as str]
            [input :refer [load-day-input]]))


(defn parse-sensor-str [sensor-str]
  (let [[_ & coords-strs] (re-find
                      #"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)"
                      sensor-str)]
    (if (nil? coords-strs)
      (throw (ex-info "Could not parse sensor string" { :sensor-str sensor-str }))
      (let [[sx sy bx by] (map parse-long coords-strs)]
        { :sensor [sx sy]
          :beacon [bx by] }))))

(defn load-input []
  (->> (load-day-input 15)
       str/split-lines
       (map parse-sensor-str)))

(defn manhatten [[start-x start-y] [end-x end-y]]
  (+ (abs (- start-x end-x)) (abs (- start-y end-y))))

(defn sensor-beacons->sensor-ranges [sensor-beacons]
  (map (fn [{:keys [sensor beacon]}] [sensor (manhatten sensor beacon)])
       sensor-beacons))

(defn is-within-sensor-range? [sensor-distances [x y]]
  (some (fn [[sensor beacon-distance]] (<= (manhatten sensor [x y]) beacon-distance))
        sensor-distances))

(defn sensor-distances->bounds [sensor-ranges]
  (let [min-x (apply min (map (fn [[[x _] d]] (- x d)) sensor-ranges))
        min-y (apply min (map (fn [[[_ y] d]] (- y d)) sensor-ranges))
        max-x (apply max (map (fn [[[x _] d]] (+ x d)) sensor-ranges))
        max-y (apply max (map (fn [[[_ y] d]] (+ y d)) sensor-ranges))]
    [[min-x min-y] [max-x max-y]]))

(def part-one-row 2000000)

(defn part-one [sensor-beacons]
  (let [sensor-ranges (sensor-beacons->sensor-ranges sensor-beacons)
        [[min-x _] [max-x _]] (sensor-distances->bounds sensor-ranges)
        coords (for [x (range min-x (inc max-x))] [x part-one-row])
        nbro-sensor-covered-coords
        (count (filter #(is-within-sensor-range? sensor-ranges %) coords))
        beacon-coords
        (set (filter (fn [[_x y]] (= y part-one-row))
                     (map (fn [{beacon :beacon}] beacon) sensor-beacons)))]
    (- nbro-sensor-covered-coords (count beacon-coords))))

(def part-two-coord-max 4000000)

(defn part-two [sensor-beacons]
  (let [sensor-ranges (sensor-beacons->sensor-ranges sensor-beacons)
        ;; According to the problem description, there is just a single position
        ;; out of range of all sensors. So that position must be right outside
        ;; one of the sensor ranges (lozenges). If it weren't, there would be 
        ;; more (non-unique) positions out of range. In fact, it must be bounded
        ;; by four of these edges, or two perpendicular edges and a side of the 
        ;; playing field, or one edge and a corner of the playing field.
        ;; So, we find the intersections of all two perpendicular edges and that
        ;; covers the first two cases. We check the last case seperately.
        ;; The intersections of the perpendicular edges can be found by their
        ;; respective slope coefficients: [(b-a)/2, (a+b)/2].
        a-coefficients (set (mapcat (fn [[[x y] range]] [(+ (- y x) range 1)
                                                         (- (- y x) range 1)])
                                    sensor-ranges))
        b-coefficients (set (mapcat (fn [[[x y] range]] [(+ (+ x y) range 1)
                                                         (- (+ x y) range 1)])
                                    sensor-ranges))
        [bx by] (or
                 ;; Check two first cases
                 (some
                  identity
                  (for [a a-coefficients, b b-coefficients]
                    (let [x (int (/ (- b a) 2))
                          y (int (/ (+ a b) 2))]
                      (when (and
                             (<= 0 x part-two-coord-max) (<= 0 y part-two-coord-max)
                             (not (is-within-sensor-range? sensor-ranges [x y])))
                        [x y]))))
                 ;; Check last case
                 (some
                  identity
                  (for [x [0 part-two-coord-max], y [0 part-two-coord-max]]
                    (when (not (is-within-sensor-range? sensor-ranges [x y]))
                      [x y]))))]
    (+ (* bx part-two-coord-max) by)))

(defn -main [& _args]
  (let [input (load-input)]
    (println "Part one:" (part-one input))
    (println "Part two:" (part-two input))))
