(ns day-16
  (:require [clojure.data.priority-map :refer [priority-map]]
            [clojure.string :as str]
            [graph :refer [bfs floyd-warshall get-fw-distance]]
            [input :refer [load-day-input]]))


(defn parse-valve-str [valve-str]
  (let [[_ valve rate-str neighbors-str]
        (re-find
         #"^Valve ([A-Z]{2,}) has flow rate=(\d+); tunnels? leads? to valves? ([A-Z]{2,}(?:, [A-Z]{2,})*$)"
         valve-str)]
    (if (nil? valve)
      (throw (ex-info (str "Could not parse sensor string " valve-str) {:valve-str valve-str}))
      [(keyword valve) {:rate (parse-long rate-str)
                        :neighbors (map keyword (str/split neighbors-str #", "))}])))

(defn load-input []
  (->> (load-day-input 16)
       str/split-lines
       (map parse-valve-str)
       (into {})))

(def start-valve :AA)
(def time-to-open-valve 1)
(def part-one-available-time 30)
(def part-two-available-time (- part-one-available-time 4))

(defn get-eventual-release [valves-data available-time valve time-opened]
  (let [rate (get-in valves-data [valve :rate])
        remaining-time (- available-time time-opened)
        total-eventual-pressure-release (* rate remaining-time)]
    total-eventual-pressure-release))

(defn get-total-eventual-release [valves-data available-time opened-times]
  (apply + (map
            (fn [[valve time-opened]]
              (get-eventual-release available-time valves-data valve time-opened))
            opened-times)))

(defrecord State [my-location time opened-times])

(defn update-part-one-record [valves-data part-one-best {:keys [opened-times]}]
  (max part-one-best (get-total-eventual-release part-one-available-time valves-data opened-times)))

(defn update-part-two-record
  [valves-data eventual-releases-by-open-ordering {:keys [opened-times]}]
  (if (or (empty? opened-times) (> (apply max (vals opened-times)) part-two-available-time))
    eventual-releases-by-open-ordering
    (let [opened-valves (set (map key (sort-by val opened-times)))
          eventual-release (get-total-eventual-release part-two-available-time valves-data opened-times)
          prev-best (get eventual-releases-by-open-ordering opened-valves)]
      (if (or (nil? prev-best) (> eventual-release prev-best))
        (assoc eventual-releases-by-open-ordering opened-valves eventual-release)
        eventual-releases-by-open-ordering))))

(defn best-open-ordering-combination [open-ordering-and-eventual-release]
  (->> open-ordering-and-eventual-release
       (keep-indexed
        (fn [idx [my-open-ordering my-best-total-eventual]]
          (let [elephant-best-total-eventual 
                (->> (subvec open-ordering-and-eventual-release (inc idx))
                     (keep
                      (fn [[elephant-open-ordering elephant-total-eventual]]
                        (when (not (some #(contains? my-open-ordering %) elephant-open-ordering))
                          elephant-total-eventual)))
                     first)]
            (when elephant-best-total-eventual
              (+ my-best-total-eventual elephant-best-total-eventual)))))
       (apply max)))

(defn both-parts [valves-data]
  (let [valves (sort (keys valves-data))
        valve-idxs (->> valves
                        (map-indexed (fn [valve idx] [idx valve]))
                        (into {}))
        valves-from-to (floyd-warshall
                        valves
                        (fn [valve] (get-in valves-data [valve :neighbors]))
                        (constantly 1))
        get-distance #(get-fw-distance valves-from-to valve-idxs %1 %2)
        usefull-valves (set (map key (filter #(-> % val :rate (> 0)) valves-data)))
        [_ [part-one-best eventual-releases-by-open-ordering]]
        (bfs
         (fn [{:keys [time opened-times] current-valve :my-location}]
           (keep
            (fn [neighbor-valve]
              (let [time-to-move-to-valve (get-distance current-valve neighbor-valve)
                    time-opened (+ time time-to-move-to-valve time-to-open-valve)]
                (when (<= time-opened part-one-available-time)
                  (State. neighbor-valve time-opened (assoc opened-times neighbor-valve time-opened)))))
            (apply disj usefull-valves (keys opened-times))))
         (State. start-valve 0 {})
         ;; Never stop searching (unless open set is exhausted)
         (constantly false)
         ;; Record keeping:
         ;;   Part one: best total eventual pressure release
         ;;   Part two: queue of valve opening orders within
         ;;             part-two-available-time, ordered by eventual pressure
         ;;             release
         [0 (priority-map)]
         (fn [[part-one-best eventual-releases-by-open-ordering] state]
           [(update-part-one-record valves-data part-one-best state)
            (update-part-two-record valves-data eventual-releases-by-open-ordering state)]))
        open-ordering-and-eventual-release
        (best-open-ordering-combination (vec (rseq eventual-releases-by-open-ordering)))]
    [part-one-best open-ordering-and-eventual-release]))

(defn -main [& _args]
  (let [input (load-input)
        [part-one-solution part-two-solution] (both-parts input)]
    (println "Part one:" part-one-solution)
    (println "Part two:" part-two-solution)))
