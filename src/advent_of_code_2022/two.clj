(ns advent-of-code-2022.two
  (:require [clojure.string :as str]))

(def data
  (map 
   (fn [x] (str/split x #"\s"))
   (str/split-lines (slurp "resources/two.txt"))))

(defn shape-score-first [x] (case (get x 1) "X" 1 "Y" 2 "Z" 3))

(defn outcome-score-first
  [[x y]]
  (case x
    "A" (case y "X" 3 "Y" 6 "Z" 0)
    "B" (case y "X" 0 "Y" 3 "Z" 6)
    "C" (case y "X" 6 "Y" 0 "Z" 3)))

(defn total-score-first
  [x]
  (+ (shape-score-first x) (outcome-score-first x)))

(def first-result (apply + (map total-score-first data)))

(defn shape-score-second
  [[x y]]
  (case x
    "A" (case y "X" 3 "Y" 1 "Z" 2)
    "B" (case y "X" 1 "Y" 2 "Z" 3)
    "C" (case y "X" 2 "Y" 3 "Z" 1)))

(defn outcome-score-second [x] (case (get x 1) "X" 0 "Y" 3 "Z" 6))

(defn total-score-second
  [x]
  (+ (shape-score-second x) (outcome-score-second x)))

(def second-result (apply + (map total-score-second data)))