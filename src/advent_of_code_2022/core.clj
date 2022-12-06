(ns advent-of-code-2022.core
  (:gen-class)
  (:require [advent-of-code-2022.one :as one]
            [advent-of-code-2022.two :as two]
            [advent-of-code-2022.three :as three]
            [advent-of-code-2022.four :as four]
            [advent-of-code-2022.five :as five]
            [clojure.string :as str]))

(defn- run
  [n]
  (let [[x y] (case n
                1 (list (str one/first-result) (str one/second-result))
                2 (list (str two/first-result) (str two/second-result))
                3 (list (str three/first-result) (str three/second-result))
                4 (list (str four/first-result) (str four/second-result))
                5 (list five/first-result five/second-result)
                (list "" ""))]
    (format "%3d. First: %9s  Second: %9s" n x y)))

(defn -main
  "Advent of Code 2022"
  [& args]
  (let [N (if (empty? args) 0 (Integer/parseInt (first args)))]
    (println (if (< N 1) 
               (str/join "\n" (map run (range 1 6)))
               (run N)))))
