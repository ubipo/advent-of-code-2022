(ns input
  (:require [clj-http.client :as http]
            [clojure.java.io :refer [make-parents]]))

(defn load-day-input [day]
  (let [day-cache-filename (format "input/day_%02d.txt" day)]
    (make-parents day-cache-filename)
    (try
      (slurp day-cache-filename)
      (catch java.io.FileNotFoundException _
        (->> (slurp "session_cookie")
            (#(http/get
               (str "https://adventofcode.com/2022/day/" day "/input")
               {:headers {:Cookie (str "session=" %)}}))
            :body
            (#(let [day-input %] 
                (spit day-cache-filename day-input) 
                day-input)))))))
