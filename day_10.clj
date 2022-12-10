(ns day-10
  (:require [clojure.string :as str]
            [input :refer [load-day-input]]))


(defn load-input []
  (->> (load-day-input 10)
       str/split-lines
       (map #(str/split % #"\s+"))))

(defn execute-instructions [instructions]
  (reduce
   (fn
     [x-states [op operand]]
     (let [x-reg (last x-states)]
       (apply conj x-states
              (case op
                "noop" [x-reg]
                "addx" [x-reg (+ x-reg (Integer/parseInt operand))]))))
   [1]
   instructions))

(defn part-one [instructions]
  (->> instructions
       execute-instructions 
       (map-indexed (fn [cycle-idx x-reg] [(inc cycle-idx) x-reg]))
       (drop 19) ;; Drop untill cycle 20 (1-indexed elvish tech)
       (take-nth 40)
       (map #(apply * %))
       (apply +)))

(def crt-width 40)
(def crt-height 6)

(defn part-two [instructions]
  (->> instructions
       execute-instructions
       (map-indexed vector)
       (reduce
        (fn [crt-pixels [cycle-idx x]]
          (if (<= (abs (- (mod cycle-idx crt-width) x)) 1) 
            (assoc crt-pixels cycle-idx \#)
            crt-pixels))
        (vec (repeat (* crt-width crt-height) \.)))
       (partition crt-width)
       (map str/join)
       (str/join "\n")
       (str "\n")))

(defn -main [& _args]
  (let [input (load-input)]
    (println "Part one:" (part-one input))
    (println "Part two:" (part-two input))))
