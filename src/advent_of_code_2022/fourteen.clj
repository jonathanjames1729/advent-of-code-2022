(ns advent-of-code-2022.fourteen
  (:require [clojure.string :as str]))

(def ^:private data
  (->> "resources/fourteen.txt"
       slurp
       str/split-lines
       (map (fn [line]
              (map (fn [coord]
                     (vec (map (fn [n] (Integer/parseInt n))
                               (str/split coord #","))))
                   (str/split line #"\s*->\s*"))))))

(defn- vertical-line
  [x y1 y2]
  (map (fn [y] [x y])
       (range y1 (inc y2))))

(defn- horizontal-line
  [y x1 x2]
  (map (fn [x] [x y])
       (range x1 (inc x2))))

(defn- line
  [x1 y1 x2 y2]
  (cond
    (= x1 x2) (if (< y1 y2)
                (vertical-line x1 y1 y2)
                (vertical-line x1 y2 y1))
    (= y1 y2) (if (< x1 x2)
                (horizontal-line y1 x1 x2)
                (horizontal-line y1 x2 x1))
    :else (println x1 y1 x2 y2)))

(def ^:private rocks
  (reduce (fn [memo rock-lines]
            (first (reduce (fn [[memo2 [x1 y1]] [x2 y2]]
                      [(apply conj memo2 (line x1 y1 x2 y2))
                       [x2 y2]])
                    [memo (first rock-lines)]
                    (rest rock-lines))))
          #{}
          data))

(def ^:private max-depth
  (reduce (fn [depth [_ y]] (if (> y depth) y depth)) 0 rocks))

(defn- drop-sand
  [rocks-and-sand]
  (loop [[x y] [500 0]]
    (let [down [x (inc y)]
          down-left [(dec x) (inc y)]
          down-right [(inc x) (inc y)]
          position (cond
                     (> y max-depth) nil
                     (not (contains? rocks-and-sand
                                     down)) down
                     (not (contains? rocks-and-sand
                                     down-left)) down-left
                     (not (contains? rocks-and-sand
                                     down-right)) down-right
                     :else nil)]
      (if (nil? position) [x y] (recur position)))))

(def first-result
  (loop [units 0
         rocks-and-sand rocks]
    (let [[x y] (drop-sand rocks-and-sand)]
      (if (> y max-depth)
        units
        (recur (inc units) (conj rocks-and-sand [x y]))))))

(def second-result
  (loop [units 0
         rocks-and-sand rocks]
    (let [[x y] (drop-sand rocks-and-sand)]
      (if (and (= x 500) (= y 0))
        (inc units)
        (recur (inc units) (conj rocks-and-sand [x y]))))))
