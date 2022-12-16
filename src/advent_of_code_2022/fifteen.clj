(ns advent-of-code-2022.fifteen
  (:require [clojure.string :as str]))

(def ^:private pattern
  (re-pattern "^Sensor at\\s+x=(-?\\d+),\\s+y=(-?\\d+):\\s+closest beacon is at\\s+x=(-?\\d+),\\s+y=(-?\\d+)\\s*$"))

(defn- absolute-difference [a b]
  (if (> a b) (- a b) (- b a)))

(defn- distance [[x1 y1] [x2 y2]]
  (+ (absolute-difference x1 x2)
     (absolute-difference y1 y2)))

(def ^:private data
  (->> "resources/fifteen.txt"
       slurp
       str/split-lines
       (map (fn [line]
              (->> line
                   (re-find pattern)
                   rest
                   (map (fn [n] (Integer/parseInt n)))
                   ((fn [[x1 y1 x2 y2]]
                      {:sensor [x1 y1]
                       :beacon [x2 y2]
                       :distance (distance [x1 y1] [x2 y2])})))))))

(defn- sensor-excluded-positions [y sensor]
  (let [[sensor-x sensor-y] (sensor :sensor)
        [beacon-x beacon-y] (sensor :beacon)
        distance (sensor :distance)
        vertical (absolute-difference y sensor-y)]
    (if (> vertical distance)
      nil
      (let [remainder (- distance vertical)]
        [(- sensor-x remainder)
         (+ sensor-x remainder)
         (if (= y beacon-y) beacon-x nil)]))))

(defn- add-line [[least greatest] lines]
  (if (empty? lines)
    (cons [least greatest] lines)
    (->> lines
         (reduce (fn [[result least1 greatest1] [least2 greatest2]]
                   (cond
                     (and (<= least1 least2)
                          (<= greatest2 greatest1)) [result least1 greatest1]
                     (and (<= least2 least1)
                          (<= greatest1 greatest2)) [result least2 greatest2]
                     (and (<= least1 least2)
                          (<= least2 greatest1)) [result least1 greatest2]
                     (and (<= least2 least1)
                          (<= least1 greatest2)) [result least2 greatest1]
                     :else [(cons [least2 greatest2] result) least1 greatest1]))
                 ['() least greatest])
         ((fn [[result least1 greatest1]]
            (cons [least1 greatest1] result))))))

(defn- excluded-positions [y]
  (->> data
       (map (partial sensor-excluded-positions y))
       (filter some?)
       (reduce (fn [[lines beacons] [least-x greatest-x beacon-x]]
                 [(add-line [least-x greatest-x]
                            lines)
                  (if (nil? beacon-x)
                    beacons
                    (conj beacons beacon-x))])
               ['() #{}])))

(def first-result
  (->> 2000000
       excluded-positions
       ((fn [[lines beacons]]
          (reduce (fn [memo [least greatest]]
                    (+ memo (inc (- greatest least))))
                  (- (count beacons))
                  lines)))))

(defn- included-positions [least-x greatest-x excluded-lines]
  (reduce (fn [memo [least greatest]]
            (mapcat (fn [[least1 greatest1]]
                      (cond
                        (and (<= least least1)
                             (<= greatest1 greatest)) '()
                        (and (< least1 least)
                             (< greatest greatest1)) (list [least1 (dec least)]
                                                           [(inc greatest) greatest1])
                        (and (<= least1 greatest)
                             (< greatest greatest1)) (list [(inc greatest) greatest1])
                        (and (< least1 least)
                             (<= least greatest1)) (list [least1 (dec least)])
                        :else (list [least1 greatest1])
                        ))
                    memo))
          (list [least-x greatest-x])
          excluded-lines))

(def second-result
  (->> (range 0 4000001)
       (reduce (fn [memo y]
                 (->> y
                      excluded-positions
                      first
                      (included-positions 0 4000000)
                      (reduce (fn [memo1 [least-x greatest-x]]
                                (reduce (fn [memo2 x]
                                          (conj memo2
                                                (+ (* 4000000 x) y)))
                                        memo1
                                        (range least-x
                                               (inc greatest-x))))
                              memo)))
               #{})
       first))
