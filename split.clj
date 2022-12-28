(ns split)


(defn split-by
  "Returns a vector of vectors for each group in coll seperated by elements for 
   which pred returns logical true, excluding the seperator element itself"
  [pred coll]
  (reduce
   (fn [acc x]
     (if (pred x)
       (if (seq (peek acc)) (conj acc []) acc)
       (conj (pop acc) (conj (peek acc) x))))
   [[]] coll))

(defn split-by-inclusive
  "Returns a vector of vectors for each group in coll seperated by elements for 
   which pred returns logical true, including the seperator element in the next
   group"
  [pred coll]
  (reduce
   (fn [acc x]
     (if (pred x)
       (conj acc [x])
       (conj (pop acc) (conj (peek acc) x))))
   [] coll))

(defn split-on
  "Returns a vector of vectors for each group in coll seperated by element (
   excluding the element itself)"
  [element coll]
  (split-by (partial = element) coll))
