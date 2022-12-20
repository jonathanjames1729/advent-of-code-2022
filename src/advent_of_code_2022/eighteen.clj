(ns advent-of-code-2022.eighteen
  (:require [clojure.set :as set])
  (:require [clojure.string :as str]))

(def data ^:private
  (->> "resources/eighteen.txt"
       slurp
       str/split-lines
       (map (fn [line]
              (->> (str/split line #",")
                   (map (fn [n] (Integer/parseInt n)))
                   vec)))
       set))

(def extent
  (->> data
       (reduce (fn [[x-min x-max
                     y-min y-max
                     z-min z-max]
                    [x y z]]
                 [(min x x-min) (max x x-max)
                  (min y y-min) (max y y-max)
                  (min z z-min) (max z z-max)])
               (vec (repeat 6 0)))
       ((fn [[x-min x-max y-min y-max z-min z-max]]
          [(dec x-min) (inc x-max)
           (dec y-min) (inc y-max)
           (dec z-min) (inc z-max)]))))

(defn- adjacent-cubes [[x y z]]
  (let [[x-min x-max y-min y-max z-min z-max] extent]
    (set (concat (cond
                   (= x x-min) (list [(inc x) y z])
                   (= x x-max) (list [(dec x) y z])
                   :else (list [(dec x) y z]
                               [(inc x) y z]))
                 (cond
                   (= y y-min) (list [x (inc y) z])
                   (= y y-max) (list [x (dec y) z])
                   :else (list [x (dec y) z]
                               [x (inc y) z]))
                 (cond
                   (= z z-min) (list [x y (inc z)])
                   (= z z-max) (list [x y (dec z)])
                   :else (list [x y (dec z)]
                               [x y (inc z)]))))))

(defn- surface-area [cubes]
  (reduce (fn [memo cube]
            (+ memo
               (- 6
                  (count (set/intersection (adjacent-cubes cube)
                                           cubes)))))
          0
          cubes))

(def first-result
  (surface-area data))

(def ^:private submerged
  (let [[x-min _ y-min _ z-min _] extent]
    (loop [cubes data
           to-add #{[x-min y-min z-min]}]
      (let [next-to-add (set/difference
                         (reduce (fn [memo cube]
                                   (set/union (adjacent-cubes cube)
                                              memo))
                                 #{}
                                 to-add)
                         cubes)]
        (if (empty? next-to-add)
          cubes
          (recur (set/union cubes to-add) next-to-add))))))

(def second-result
  (let [[x-min x-max y-min y-max z-min z-max] extent
        x-side (inc (- x-max x-min))
        y-side (inc (- y-max y-min))
        z-side (inc (- z-max z-min))]
  (- (surface-area (set/difference submerged data))
      (+ (* 2 x-side y-side)
         (* 2 y-side z-side)
         (* 2 z-side x-side)))))
