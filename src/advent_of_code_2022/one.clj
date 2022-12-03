(ns advent-of-code-2022.one
  (:require [clojure.string :as str]))

(def data (str/split-lines (slurp "resources/one.txt")))

(def totals
  (reduce (fn [[head & tail] x] 
            (if (empty? x) 
              (list* 0 head tail)
              (cons (+ head (Integer/parseInt x)) tail)))
          '(0)
          data))

(def first-result (apply max totals))

(def second-result (apply + (take 3 (sort > totals))))
