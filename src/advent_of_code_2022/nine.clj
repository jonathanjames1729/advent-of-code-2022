(ns advent-of-code-2022.nine
  (:require [clojure.string :as str]))

(def ^:private data
  (->> "resources/nine.txt"
       slurp
       str/split-lines
       (reduce (fn [memo line]
                 (let [[direction distance] (str/split line #"\s+")
                       steps (Integer/parseInt distance)]
                   (->> (case direction
                          "R" [1 0]
                          "U" [0 1]
                          "L" [-1 0]
                          "D" [0 -1])
                        (repeat steps)
                        (concat memo))))
               '())))

(defn- follow
  [[leader-x leader-y] follower]
  (let [[follower-x follower-y] follower
        delta-x (- follower-x leader-x)
        delta-y (- follower-y leader-y)]
    (cond
      (and (contains? #{-1 0 1} delta-x)
           (contains? #{-1 0 1} delta-y)) follower
      (zero? delta-x) [follower-x
                       (if (pos? delta-y)
                         (dec follower-y)
                         (inc follower-y))]
      (zero? delta-y) [(if (pos? delta-x)
                         (dec follower-x)
                         (inc follower-x))
                       follower-y]
      :else [(if (pos? delta-x)
               (dec follower-x)
               (inc follower-x))
             (if (pos? delta-y)
               (dec follower-y)
               (inc follower-y))])))

(def first-result
  (->> data
       (reduce (fn [[[head-x head-y] tail tails] [x y]]
                 (let [new-head [(+ head-x x) (+ head-y y)]
                       new-tail (follow new-head tail)]
                   [new-head new-tail (conj tails new-tail)]))
               [[0 0] [0 0] #{[0 0]}])
       last
       count))

(def second-result
  (->> data
       (reduce (fn [[[[head-x head-y] & knots] tails] [x y]]
                 (let [new-head [(+ head-x x) (+ head-y y)]]
                   (->> knots
                        (reduce (fn [[memo previous-knot] knot]
                                  (let [new-knot (follow previous-knot knot)]
                                    [(cons new-knot memo) new-knot]))
                                [(list new-head) new-head])
                        ((fn [[new-knots new-tail]]
                           [(reverse new-knots) (conj tails new-tail)])))))
               [(repeat 10 [0 0]) #{[0 0]}])
       last
       count))
