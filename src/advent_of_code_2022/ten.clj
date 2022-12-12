(ns advent-of-code-2022.ten
  (:require [clojure.string :as str]))

(defn- parse-line
  [line]
  (if-let [n (->> line
                  (re-find #"^\s*(?:addx\s+(-?\d+))|(?:noop)\s*$")
                  second)]
    (Integer/parseInt n)
    nil))

(def ^:private data
  (map parse-line
       (str/split-lines (slurp "resources/ten.txt"))))

(def ^:private x-during-cycle
  (->> data
       (reduce (fn [[result x] arg]
                 (if (nil? arg)
                   [(cons x result) x]
                   [(cons x (cons x result)) (+ x arg)]))
               ['() 1])
       first
       reverse))

(def ^:private x-during-cycle-vector
  (vec (cons nil x-during-cycle)))

(def ^:private cycles-of-interest
  '(20 60 100 140 180 220))

(def first-result
  (reduce (fn [score cycle]
            (+ score (* cycle (get x-during-cycle-vector cycle))))
          0
          cycles-of-interest))

(def second-result
  (->> x-during-cycle
       (map-indexed (fn [index x]
                      (let [position (mod index 40)]
                        (if (and (>= position (dec x))
                                 (<= position (inc x)))
                          \#
                          \.))))
       (partition 40)
       (map str/join)
       (str/join "\n")))
