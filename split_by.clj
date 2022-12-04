(ns split-by)

;; Split array when predicate is true (excluding the element that satisfies the
;; predicate itself, unlike clojure.core/split-with) 
(defn split-by
  [pred coll]
  (reduce
   (fn [acc x]
     (if (pred x)
       (if (seq (peek acc)) (conj acc []) acc)
       (conj (pop acc) (conj (peek acc) x))))
   [[]] coll))
