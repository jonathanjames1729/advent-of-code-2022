(ns advent-of-code-2022.five
  (:require [clojure.string :as str]))

(def ^:private data
  (str/split-lines (slurp "resources/five.txt")))

(def ^:private stacks
  (reduce (fn [memo line]
            (map-indexed (fn [i  stack]
                           (let [crate (get line (+ 1 (* i 4)))]
                             (if (= crate \ )
                               stack
                               (cons crate stack))))
                         memo))
          (repeat 9 '())
          (reverse (take 8 data)) ))

(def ^:private steps
  (map (fn [line]
         (map (fn [n] (Integer/parseInt n))
              (drop 1 (re-find #"move (\d+) from (\d) to (\d)"
                               line))))
       (drop 10 data)))

(defn- move-crates-first
  [state [times start end]]
  (assoc state
         start (drop times (get state start))
         end (concat (reverse (take times (get state start)))
                     (get state end))))

(defn- move-crates-second
  [state [times start end]]
  (assoc state
         start (drop times (get state start))
         end (concat (take times (get state start))
                     (get state end))))

(defn- final-stacks
  [move-crates]
  (reduce move-crates
          (vec (cons nil stacks))
          steps))

(defn- top-crates
  [move-crates]
  (str/join (map first
                 (rest (final-stacks move-crates)))))

(def first-result (top-crates move-crates-first))

(def second-result (top-crates move-crates-second))
