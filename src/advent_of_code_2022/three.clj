(ns advent-of-code-2022.three
  (:require [clojure.string :as str]))

(def ^:private data
  (str/split-lines (slurp "resources/three.txt")))

(defn- score
  [char]
  (let [code (int char)]
    (if (> code 96) (- code 96) (+ (- code 64) 26))))

(def first-result
  (apply + (map (fn [s] 
                  (loop [half (/ (count s) 2)
                         [head & tail] (seq s)]
                    (if (>= (str/last-index-of s head) half)
                      (score head)
                      (recur half tail))))
                data)))

(def second-result
  (loop [[line1 line2 line3 & rest] data
         total 0]
    (let [next-total (loop [[head & tail] line1]
                       (if (and (str/includes? line2 (str head))
                                (str/includes? line3 (str head)))
                         (+ total (score head))
                         (recur tail)))]
      (if (< (count rest) 3)
        next-total
        (recur rest next-total)))))
