(ns split)

;; Split array when predicate returns logical true (excluding the element that 
;; satisfies the predicate itself, unlike clojure.core/split-with) 
(defn split-by
  [pred coll]
  (reduce
   (fn [acc x]
     (if (pred x)
       (if (seq (peek acc)) (conj acc []) acc)
       (conj (pop acc) (conj (peek acc) x))))
   [[]] coll))


;; Split array on all occurrences of element (excluding the element itself)
(defn split-on
  [element coll]
  (split-by (partial = element) coll))
