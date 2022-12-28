(ns graph
  (:require [clojure.data.priority-map :refer [priority-map]]))


(defn safe-pop
  "Assumes coll cannot contain nil."
  [coll]
  (let [first (first coll)]
    (if (nil? first) [nil coll] [first (pop coll)])))

(defn positions
  "Returns the indices of the elements in coll that satisfy pred."
  [pred coll]
  (keep-indexed (fn [idx x] (when (pred x) idx)) coll))

(defn index-of
  "Returns the index of the first occurrence of item in coll."
  [item coll]
  (first (positions #{item} coll)))

(defn reconstruct-dijkstra-path [came-from to]
  (loop [path [to]]
    (let [[parent _] (get came-from (last path))]
      (if (nil? parent) path (recur (conj path parent))))))

(defn dijkstra [get-neighbors w start is-end?]
  (loop [open-set (priority-map start 0)
         came-from {start [nil 0]}]
    (let [[[node _] open-set] (safe-pop open-set)
          [_ gurrent-g-score] (get came-from node)]
      (cond
        (is-end? node) (reconstruct-dijkstra-path came-from node)
        (= node nil) (throw (ex-info "No path found" {:last-node node}))
        :else (let
               [[open-set came-from]
                (reduce
                 (fn [[open-set came-from] neighbor]
                   (let [[_ old-cost] (get came-from neighbor)
                         new-cost (+ gurrent-g-score (w node neighbor))]
                     (if (or (nil? old-cost) (< new-cost old-cost))
                       [(assoc open-set neighbor new-cost)
                        (assoc came-from neighbor [node new-cost])]
                       [open-set came-from])))
                 [open-set came-from]
                 (get-neighbors node))]
                (recur open-set came-from))))))

(defn a* [get-neighbors w h start is-end?]
  (loop [open-set (priority-map start (h start))
         came-from {start [nil 0]}]
    (let [[[node _] open-set] (safe-pop open-set)
          [_ gurrent-g-score] (get came-from node)
          neighbors (get-neighbors node)]
      (cond
        (is-end? node) (reconstruct-dijkstra-path came-from node)
        (= node nil) (throw (ex-info "No path found" {:last-node node}))
        :else (let
               [[open-set came-from]
                (reduce
                 (fn [[open-set came-from] neighbor]
                   (let [[_ old-cost] (get came-from neighbor)
                         new-cost (+ gurrent-g-score (w node neighbor))]
                     (if (or (nil? old-cost) (< new-cost old-cost))
                       [(assoc open-set neighbor (+ new-cost (h neighbor)))
                        (assoc came-from neighbor [node new-cost])]
                       [open-set came-from])))
                 [open-set came-from]
                 neighbors)]
                (recur open-set came-from))))))

(defn bfs [get-neighbors start is-end? initial-state update-state]
  (loop [open-queue (list start)
         came-from {start nil}
         state initial-state]
    (let [[node open-queue] (safe-pop open-queue)]
      (if (nil? node)
        [nil state]
        (let [state (update-state state node)]
          (if (is-end? node)
            [(reconstruct-dijkstra-path came-from node) state]
            (let
             [[open-queue came-from]
              (reduce
               (fn [[open-queue came-from] neighbor]
                 (if (contains? came-from neighbor)
                   [open-queue came-from]
                   [(conj open-queue neighbor)
                    (assoc came-from neighbor node)]))
               [open-queue came-from]
               (get-neighbors node))]
              (recur open-queue came-from state))))))))

(defn reconstruct-fw-path [from-to from to]
  (loop [path [from]]
    (let [[_ next] (get-in from-to [(last path) to])]
      (when (nil? next) (throw (ex-info "No path found" {:from from :to to})))
      (if (= next to) path (recur (conj path next))))))

(defn get-fw-distance [from-to node-idxs from to]
  (let [[distance _] (get-in from-to [(get node-idxs from) (get node-idxs to)])]
    distance))

(defn floyd-warshall [nodes get-neighbors w]
  (let [from-to (vec (repeat (count nodes) (vec (repeat (count nodes) [nil nil]))))
        from-to (reduce
                 (fn [from-to [i node]]
                   (as-> from-to $
                     (assoc-in $ [i i] [0 i])
                     (reduce
                      (fn [from-to neighbor-node]
                        (let [j (index-of neighbor-node nodes)]
                          (assoc-in from-to [i j] [(w node neighbor-node) j])))
                      $
                      (get-neighbors node))))
                 from-to
                 (map-indexed vector nodes))]
    (reduce
     (fn [from-to [k i j]]
       (let [[i>j-w _] (get-in from-to [i j])
             [i>k-w i>k-next] (get-in from-to [i k])
             [k>j-w _] (get-in from-to [k j])]
         (if (or (nil? i>k-w) (nil? k>j-w))
           from-to
           (let [i>j-via-k-w (+ i>k-w k>j-w)]
             (if (or (nil? i>j-w) (< i>j-via-k-w i>j-w))
               (assoc-in from-to [i j] [i>j-via-k-w i>k-next])
               from-to)))))
     from-to
     (for [k (range (count nodes))
           i (range (count nodes))
           j (range (count nodes))] [k i j]))))
