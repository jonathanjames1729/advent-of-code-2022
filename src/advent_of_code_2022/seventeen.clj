(ns advent-of-code-2022.seventeen
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def ^:private data
  (->> "resources/seventeen.txt"
       slurp
       str/trim
       (map (fn [char]
              (case char
                \< -1
                \> 1)))
       repeat
       (apply concat)))

(def ^:private all-rocks
  (->> (list #{[3 4] [4 4] [5 4] [6 4]}
             #{[4 4] [3 5] [4 5] [5 5] [4 6]}
             #{[3 4] [4 4] [5 4] [5 5] [5 6]}
             #{[3 4] [3 5] [3 6] [3 7]}
             #{[3 4] [4 4] [3 5] [4 5]})
       repeat
       (apply concat)))

(def ^:private empty-chamber
  #{[1 0] [2 0] [3 0] [4 0] [5 0] [6 0] [7 0]})

(defn- formation-height [chamber]
  (reduce (fn [height [_ y]]
            (max height y))
          0
          chamber))

(defn- blow-rock [chamber rock direction]
  (let [blown-rock (into #{} (map (fn [[x y]] [(+ x direction) y]) rock))]
    (if (reduce (fn [result [x _]] (or result (< x 1) (> x 7))) false blown-rock)
      rock
      (if (empty? (set/intersection chamber blown-rock))
        blown-rock
        rock))))

(defn- place-rock [chamber rock]
  (let [updated-chamber (set/union chamber rock)
        [min-y max-y] (reduce (fn [[miny maxy] [_ y]]
                                [(min y miny) (max y maxy)])
                              [0 0]
                              rock)
        least-y (reduce (fn [result y]
                          (if (and (nil? result)
                                   (= 7
                                      (count (set/intersection updated-chamber
                                                               (into #{}
                                                                     (map (fn [x]
                                                                            [x y]))
                                                                     (range 1 8))))))
                            y
                            result))
                        nil
                        (range min-y (inc max-y)))]
    (if (nil? least-y)
      updated-chamber
      (into #{}
            (filter (fn [[_ y]]
                      (>= y least-y))
                    updated-chamber)))))

(defn- add-rock [chamber rock directions]
  (let [height (formation-height chamber)
        inserted-rock (into #{}
                            (map (fn [[x y]]
                                   [x (+ y height)])
                                 rock))]
    (loop [[direction & next-directions] directions
           moved-rock inserted-rock]
      (let [blown-rock (blow-rock chamber moved-rock direction)
            dropped-rock (into #{} (map (fn [[x y]] [x (dec y)]) blown-rock))]
        (if (seq (set/intersection chamber dropped-rock))
          [(place-rock chamber blown-rock) next-directions]
          (recur next-directions dropped-rock))))))

(defn- add-rocks [n]
  (loop [chamber empty-chamber
         [rock & rocks] all-rocks
         directions data
         index 0]
    (if (= (mod index 1000) 0) (println index) nil)
    (if (= index n)
      (formation-height chamber)
      (let [[updated-chamber next-directions] (add-rock chamber rock directions)]
        (recur updated-chamber
               rocks
               next-directions
               (inc index))))))

(def first-result (add-rocks 2022))

(def second-result 0)
