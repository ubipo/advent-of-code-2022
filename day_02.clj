(ns day-02
  (:require [clojure.string :as string]
            [input :refer [load-day-input]]
            [split :refer [split-on]]))


;; For part one, the second column (X/Y/Z) is my played shape
;; 0/A/X: Rock
;; 1/B/Y: Paper
;; 2/C/Z: Scissors
;; 
;; Game is won iff my play is exactly one higher than my adversary's:
;;  game-won = (me - adversary) % 3 == 1
;;
;; For part two, the second column (X/Y/Z) is the desired game outcome
;; 0/X: Lose
;; 1/Y: Tie
;; 2/Z: Win

(defn parse-round [round-string]
  (zipmap [:adversary-shape :second-column]
          (map (fn [min-char char] (- (int char) (int min-char)))
           [\A \X]
           (map first (split-on \space (char-array round-string))))))

(defn load-input []
  (map parse-round (string/split-lines (load-day-input 2))))

(defn interpret-round
  "Interpret a round for part one or two by remapping the :second-column"
  [round second-column-key]
  (assoc round
         second-column-key (:second-column round)))

(defn round-outcome
  "Outcome of the round as a number: 0 for a tie, 1 for a win, 2 for a loss"
  [round]
  (mod (- (:my-shape round) (:adversary-shape round)) 3))

(defn round-score-part-one
  "Score for the round: (my shape score) + (round outcome score)"
  [round]
  (+
   (+ (:my-shape round) 1)
   (case (round-outcome round)
     0 3
     1 6
     2 0)))

(defn part-one
  "Total score assuming the second column is the shape I play"
  [input]
  (reduce + (map #(round-score-part-one (interpret-round % :my-shape)) input)))

(defn my-shape
  "My choice of shape given a round from part two"
  [round]
  (mod (+ (:adversary-shape round) (- (:outcome round) 1)) 3))

(defn round-score-part-two
  [round] 
  (+ 
   (+ (my-shape round) 1) 
   (case (:outcome round) 
     0 0
     1 3
     2 6)))

(defn part-two
  "Total score assuming the second column is the desired round outcome"
  [input]
  (reduce + (map #(round-score-part-two (interpret-round % :outcome)) input)))

(defn -main [& _args] 
  (let [input (load-input)] 
    (println "Part one: " (part-one input)) 
    (println "Part two: " (part-two input))))
