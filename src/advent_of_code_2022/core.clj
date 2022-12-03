(ns advent-of-code-2022.core
  (:gen-class)
  (:require [advent-of-code-2022.one :as one]
            [advent-of-code-2022.two :as two]))

(defn -main
  "Advent of Code 2022"
  [& args]
  (case (first args)
    "1" (println
         "First:" one/first-result
         "Second:" one/second-result)
    "2" (println
         "First:" two/first-result
         "Second:" two/second-result)))