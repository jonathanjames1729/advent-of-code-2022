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
            [clojure.string :as str]))

(def ^:private format-ten
  (->> ten/second-result
       str/split-lines
       (concat (list (format "%3d. First: %14s",
                             10,
                             ten/first-result)
                     "Second:"))
       (str/join "\n     ")))

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
    (list "" "")))

(defn- run
  [day]
  (let [[first-result second-result] (get-results day)]
    (if (nil? second-result)
      first-result
      (format "%3d. First: %14s  Second: %14s" day first-result second-result))))

(defn -main
  "Advent of Code 2022"
  [& args]
  (let [day (if (empty? args) 0 (Integer/parseInt (first args)))]
    (println (if (< day 1) 
               (str/join "\n" (map run (range 1 19)))
               (run day)))))
