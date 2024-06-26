(ns advent-of-code-2022.core
  (:gen-class)
  (:require [advent-of-code-2022.one :as one]
            [advent-of-code-2022.two :as two]
            [advent-of-code-2022.three :as three]
            [advent-of-code-2022.four :as four]
            [advent-of-code-2022.five :as five]
            [advent-of-code-2022.six :as six]
            [advent-of-code-2022.seven :as seven]
            [advent-of-code-2022.eight :as eight]
            [advent-of-code-2022.nine :as nine]
            [advent-of-code-2022.ten :as ten]
            [advent-of-code-2022.eleven :as eleven]
            [advent-of-code-2022.twelve :as twelve]
            [advent-of-code-2022.thirteen :as thirteen]
            [advent-of-code-2022.fourteen :as fourteen]
            [advent-of-code-2022.fifteen :as fifteen]
            [advent-of-code-2022.sixteen :as sixteen]
            [advent-of-code-2022.seventeen :as seventeen]
            [advent-of-code-2022.eighteen :as eighteen]
            [advent-of-code-2022.nineteen :as nineteen]
            [advent-of-code-2022.twenty :as twenty]
            [advent-of-code-2022.twenty_one :as twenty_one]
            [advent-of-code-2022.twenty_two :as twenty_two]
            [advent-of-code-2022.twenty_three :as twenty_three]
            [advent-of-code-2022.twenty_four :as twenty_four]
            [advent-of-code-2022.twenty_five :as twenty_five]
            [clojure.string :as str]))

(def ^:private format-ten
  (->> ten/second-result
       str/split-lines
       (concat (list (format "%3d. First: %15s"
                             10
                             ten/first-result)
                     "Second:"))
       (str/join "\n     ")))

(def ^:private format-twenty-five
  (format "%3d. %22s" 25 twenty_five/first-result))

(defn- get-results
  [day]
  (case day
    1 (list (str one/first-result) (str one/second-result))
    2 (list (str two/first-result) (str two/second-result))
    3 (list (str three/first-result) (str three/second-result))
    4 (list (str four/first-result) (str four/second-result))
    5 (list five/first-result five/second-result)
    6 (list (str six/first-result) (str six/second-result))
    7 (list (str seven/first-result) (str seven/second-result))
    8 (list (str eight/first-result) (str eight/second-result))
    9 (list (str nine/first-result) (str nine/second-result))
    10 (list format-ten nil)
    11 (list (str eleven/first-result) (str eleven/second-result))
    12 (list (str twelve/first-result) (str twelve/second-result))
    13 (list (str thirteen/first-result) (str thirteen/second-result))
    14 (list (str fourteen/first-result) (str fourteen/second-result))
    15 (list (str fifteen/first-result) (str fifteen/second-result))
    16 (list (str sixteen/first-result) (str sixteen/second-result))
    17 (list (str seventeen/first-result) (str seventeen/second-result))
    18 (list (str eighteen/first-result) (str eighteen/second-result))
    19 (list (str nineteen/first-result) (str nineteen/second-result))
    20 (list (str twenty/first-result) (str twenty/second-result))
    21 (list (str twenty_one/first-result) (str twenty_one/second-result))
    22 (list (str twenty_two/first-result) (str twenty_two/second-result))
    23 (list (str twenty_three/first-result) (str twenty_three/second-result))
    24 (list (str twenty_four/first-result) (str twenty_four/second-result))
    25 (list format-twenty-five nil)
    (list "" "")))

(defn- run
  [day]
  (let [[first-result second-result] (get-results day)]
    (if (nil? second-result)
      first-result
      (format "%3d. First: %15s  Second: %15s" day first-result second-result))))

(defn -main
  "Advent of Code 2022"
  [& args]
  (let [day (if (empty? args) 0 (Integer/parseInt (first args)))]
    (println (if (< day 1) 
               (str/join "\n" (map run (range 1 26)))
               (run day)))))
