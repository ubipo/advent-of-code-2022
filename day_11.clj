(ns day-11
  (:require [clojure.string :as str]
            [input :refer [load-day-input]]
            [split :refer [split-by]]))


(defrecord Monkey
  [items op-operator op-operand test-divisor throw-true throw-false])

(defn parse-monkey-lines [monkey-lines]
  (let [[_ items-str op-str test-str throw-true-str throw-false-str] 
        (map #(second (str/split % #":\s+")) monkey-lines)
        [_ op-operator op-operand] (re-matches #"new = old (\*|\+) (\d+|old)" op-str)]
    (Monkey. (vec (map parse-long (str/split items-str #", ")))
             op-operator
             (if (= "old" op-operand) :old (parse-long op-operand))
             (parse-long (last (str/split test-str #"\s+")))
             (parse-long (last (str/split throw-true-str #"\s+")))
             (parse-long (last (str/split throw-false-str #"\s+"))))))

(defn load-input []
  (->> (load-day-input 11)
       str/split-lines
       (split-by empty?)
       (map parse-monkey-lines)
       vec))

(defn perform-inspection [regulate-worry-level monkey monkeys item]
  (let [operand-value (if (= :old (:op-operand monkey))
                        item
                        (:op-operand monkey))
        item (case (:op-operator monkey)
               "+" (+ item operand-value)
               "*" (* item operand-value))
        item (regulate-worry-level item)
        test-result (= 0 (mod item (:test-divisor monkey)))
        dest-monkey (if test-result
                      (:throw-true monkey)
                      (:throw-false monkey))
        dest-monkey-items (get-in monkeys [dest-monkey :items])]
    (assoc-in monkeys [dest-monkey :items] (conj dest-monkey-items item))))

(defn perform-round [regulate-worry-level items-inspected monkeys]
  (reduce
   (fn [[items-inspected monkeys] monkey-idx]
     (let
      [monkey (get monkeys monkey-idx)
       items (:items monkey)
       new-monkeys (reduce
                    (partial perform-inspection regulate-worry-level monkey)
                    monkeys
                    (:items monkey))]
       [(assoc items-inspected monkey-idx (+ (get items-inspected monkey-idx) (count items)))
        (assoc-in new-monkeys [monkey-idx :items] [])]))
    [items-inspected monkeys]
    (range (count monkeys))))

(defn perform-rounds [regulate-worry-level nbro-rounds monkeys]
  (reduce
   (fn [[items-inspected monkeys] _]
     (perform-round regulate-worry-level items-inspected monkeys))
   [(vec (repeat (count monkeys) 0)) monkeys]
   (range nbro-rounds)))

(defn calc-monkey-bussiness-level [items-inspected]
  (->> items-inspected
       sort
       (take-last 2)
       (apply *)))

(defn part-one [monkeys]
  (->> monkeys
       (perform-rounds #(long (/ % 3)) 20)
       (#(let [[items-inspected _] %] items-inspected))
       calc-monkey-bussiness-level))

(defn part-two [monkeys]
  (->> monkeys
       ((fn [monkeys] 
          (let [gcd (apply * (map #(:test-divisor %) monkeys))]
            (perform-rounds #(mod % gcd) 10000 monkeys))))
       (#(let [[items-inspected _] %] items-inspected))
       calc-monkey-bussiness-level))

(defn -main [& _args]
  (let [input (load-input)]
    (println "Part one:" (part-one input))
    (println "Part two:" (part-two input))))
