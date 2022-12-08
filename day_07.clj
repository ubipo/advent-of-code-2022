(ns day-07
  (:require [clojure.string :as str]
            [input :refer [load-day-input]]
            [split :refer [split-by-inclusive]]
            [clojure.core.match :refer [match]]))


(def shell-prompt "$ ")

(defn transcript->commands [transcript]
  (->> transcript
       str/split-lines
       (split-by-inclusive #(str/starts-with? % shell-prompt))))

(defn parse-ls-line [line]
  (let [[size-str name] (str/split line #"\s+")]
    [name (if (= size-str "dir") {} (parse-long size-str))]))

(defn commands->tree [commands]
  (first
   (reduce
    (fn [[root-dir cwd] [command-line & output-lines]]
      (let [[command args] (str/split (subs command-line (count shell-prompt)) #"\s+" 2)]
        (match [command args]
          ["cd" "/"] [root-dir []]
          ["cd" ".."] [root-dir (if (empty? cwd) [] (pop cwd))]
          ["cd" _] [root-dir (conj cwd args)]
          ["ls" _] (let [children (into {} (map parse-ls-line output-lines)) 
                         new-root-dir (if (empty? cwd) children (assoc-in root-dir cwd children))] 
                     [new-root-dir cwd]))))
    [{} []]
    commands)))

(defn load-input []
  (->> (load-day-input 7)
       transcript->commands
       commands->tree))

(defn dir->sizes [dir]
  (reduce
   (fn [[size children] [_name size-or-subdir]]
     (if (map? size-or-subdir)
       (let [[subdir-size subdir-children] (dir->sizes size-or-subdir)]
         [(+ size subdir-size) (apply conj children subdir-size subdir-children)])
       [(+ size size-or-subdir) children]))
   [0 []]
   dir))

(defn part-one [tree]
  (apply + (filter #(<= % 100000) (flatten (dir->sizes tree)))))

(def fs-size 70000000)
(def update-size 30000000)

(defn part-two [input]
  (let [dir-sizes (dir->sizes input)
        size-to-clear (- update-size (- fs-size (first dir-sizes)))]
    (first (sort (filter #(>= % size-to-clear) (flatten dir-sizes))))))

(defn -main [& _args]
  (let [input (load-input)]
    (println "Part one:" (part-one input))
    (println "Part two:" (part-two input))))
