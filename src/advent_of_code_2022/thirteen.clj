(ns advent-of-code-2022.thirteen
  (:require [clojure.edn :as edn])
  (:require [clojure.string :as str]))

(def ^:private data
  (->> "resources/thirteen.txt"
       slurp
       str/split-lines
       (map (fn [line] (if (str/blank? line)
                         nil
                         (-> line
                             (str/replace #"," " ")
                             edn/read-string))))
       (reduce (fn [[pairs current] line]
                 (if (nil? line)
                   [pairs nil]
                   (if (nil? current)
                     [pairs {:left line}]
                     [(cons (assoc current :right line) pairs)
                      nil])))
               ['() nil])
       first
       reverse))

(declare compare-packets)

(defn- compare-elements
  [left right]
  (if (nil? left)
    (if (nil? right) nil true)
    (if (nil? right)
      false
      (if (sequential? left)
        (if (sequential? right)
          (compare-packets left right)
          (compare-packets left [right]))
        (if (sequential? right)
          (compare-packets [left] right)
          (cond
            (< left right) true
            (> left right) false
            :else nil))))))

(defn- compare-packets
  [left right]
  (loop [[left-head & left-rest] left
         [right-head & right-rest] right]
    (if-some [result (compare-elements left-head
                                       right-head)]
      result
      (if (and (empty? left-rest) (empty? right-rest))
        nil
        (recur left-rest right-rest)))))

(def first-result
  (->> data
       (reduce (fn [[total index] pair]
                 (if (compare-packets (pair :left)
                                      (pair :right))
                   [(+ total index) (inc index)]
                   [total (inc index)]))
               [0 1])
       first))

(def second-result
  (->> data
       (reduce (fn [memo pair]
                 (conj memo (pair :left) (pair :right)))
               (list [[2]] [[6]]))
       (sort compare-packets)
       (reduce (fn [[result index] packet]
                 (if (contains? #{[[2]] [[6]]} packet)
                   [(* result index) (inc index)]
                   [result (inc index)]))
               [1 1])
       first))
