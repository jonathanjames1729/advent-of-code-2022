(ns advent-of-code-2022.twenty
  (:require [clojure.string :as str]))

(def ^:private data
  (->> "resources/twenty.txt"
       slurp
       str/split-lines
       (map-indexed (fn [index line] 
                      {:position index
                       :value (Integer/parseInt line)}))))

(defn- move-number [numbers old-position new-position]
  (doall
   (cond
     (< old-position new-position) (map (fn [number]
                                          (let [position (number :position)]
                                            (if (and (> position
                                                        old-position)
                                                     (<= position
                                                         new-position))
                                              (assoc number :position (dec position))
                                              number)))
                                        numbers)
     (> old-position new-position) (map (fn [number]
                                          (let [position (number :position)]
                                            (if (and (>= position
                                                         new-position)
                                                     (< position
                                                        old-position))
                                              (assoc number :position (inc position))
                                              number)))
                                        numbers)
     :else numbers)))

(defn- mix-numbers [numbers]
  (let [length (dec (count numbers))]
    (loop [nose '()
           [head & tail] numbers]
      (let [{position :position value :value} head
            new-position (mod (+ position value) length)]
        (if (empty? tail)
          (reverse (cons (assoc head :position new-position)
                         (move-number nose position new-position)))
          (recur (cons (assoc head :position new-position)
                       (move-number nose position new-position))
                 (move-number tail position new-position)))))))

(defn- locate-zero [numbers]
  (get (first (filter (fn [number] (zero? (number :value))) numbers)) :position))

(defn- signature [numbers]
  (let [zero-position (locate-zero numbers)
        length (count numbers)
        positions #{(mod (+ zero-position 1000) length)
                    (mod (+ zero-position 2000) length)
                    (mod (+ zero-position 3000) length)}]
    (->> numbers
         (filter (fn [number] (contains? positions (number :position))))
         (reduce (fn [total number] (+ total (number :value))) 0))))

(def first-result
  (->> data
       mix-numbers
       signature))

(def ^:private decryption-key 811589153)

(defn- n-times [n f numbers]
  (reduce (fn [memo _] (f memo)) numbers (range n)))

(def second-result
  (->> data
       (map (fn [number]
              (assoc number
                     :value (* decryption-key
                               (number :value)))))
       (n-times 10 mix-numbers)
       signature))
