(ns split)

  
(defn split-by
  "Returns a vector of vectors for each group in coll seperated by elements for 
   which pred returns logical true (excluding the element that satisfies the 
   predicate itself, unlike clojure.core/split-with)"
  [pred coll]
  (reduce
   (fn [acc x]
     (if (pred x)
       (if (seq (peek acc)) (conj acc []) acc)
       (conj (pop acc) (conj (peek acc) x))))
   [[]] coll))

(defn split-on
  "Returns a vector of vectors for each group in coll seperated by element (
   excluding the element itself)"
  [element coll]
  (split-by (partial = element) coll))
