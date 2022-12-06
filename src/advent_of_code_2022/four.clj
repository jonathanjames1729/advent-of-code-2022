(ns advent-of-code-2022.four
  (:require [clojure.string :as str]))

(def ^:private data (map (fn [x]
                 (split-at 2 (map (fn [x] (Integer/parseInt x))
                                  (str/split x #"[-,]"))))
               (str/split-lines (slurp "resources/four.txt"))))

(defn- inside?
  [[x-lower x-upper] [y-lower y-upper]]
  (and (>= x-lower y-lower) (<= x-upper y-upper)))

(defn- either-inside?
  [x y]
  (or (inside? x y) (inside? y x)))

(def first-result
  (reduce (fn [memo [x y]]
            (if (either-inside? x y) (inc memo) memo))
          0
          data))

(defn- disjoint?
  [[x-lower x-upper] [y-lower y-upper]]
  (or (> x-lower y-upper) (> y-lower x-upper)))

(def second-result
  (reduce (fn [memo [x y]]
            (if (disjoint? x y) memo (inc memo)))
          0
          data))
