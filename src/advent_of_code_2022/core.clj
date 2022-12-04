(ns advent-of-code-2022.core
  (:gen-class)
  (:require [advent-of-code-2022.one :as one]
            [advent-of-code-2022.two :as two]
            [advent-of-code-2022.three :as three]
            [advent-of-code-2022.four :as four]
            [clojure.string :as str]))

(defn run
  [n]
  (let [[x y] (case n
                1 (list one/first-result one/second-result)
                2 (list two/first-result two/second-result)
                3 (list three/first-result three/second-result)
                4 (list four/first-result four/second-result)
                (list 0 0))]
    (format "%3d. First: %6d Second: %6d" n x y)))

(defn -main
  "Advent of Code 2022"
  [& args]
  (let [N (if (empty? args) 0 (Integer/parseInt (first args)))]
    (println (if (< N 1) 
               (str/join "\n" (map run (range 1 5)))
               (run N)))))
