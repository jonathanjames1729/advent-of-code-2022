(ns advent-of-code-2022.core
  (:gen-class)
  (:require [advent-of-code-2022.one :as one]
            [advent-of-code-2022.two :as two]
            [advent-of-code-2022.three :as three]
            [advent-of-code-2022.four :as four]
            [advent-of-code-2022.five :as five] 
            [advent-of-code-2022.six :as six] 
            [advent-of-code-2022.seven :as seven]
            [clojure.string :as str]))

(defn- get-results
  [day]
  (case day
    1 (list (str one/first-result) (str one/second-result))
    2 (list (str two/first-result) (str two/second-result))
    3 (list (str three/first-result) (str three/second-result))
    4 (list (str four/first-result) (str four/second-result))
    5 (list five/first-result five/second-result)
    6 (list (str six/first-result) (str six/second-result))
    7 (list (str seven/first-result) (str seven/second-result))
    (list "" "")))

(defn- run
  [day]
  (let [[first-result second-result] (get-results day)]
    (format "%3d. First: %9s  Second: %9s" day first-result second-result)))

(defn -main
  "Advent of Code 2022"
  [& args]
  (let [day (if (empty? args) 0 (Integer/parseInt (first args)))]
    (println (if (< day 1) 
               (str/join "\n" (map run (range 1 8)))
               (run day)))))
