(ns advent-of-code-2022.eight
  (:require [clojure.string :as str]))

(def ^:private data
  (map (fn [line]
         (map (fn [n] (Integer/parseInt n))
              (str/split line #"")))
       (str/split-lines (slurp "resources/eight.txt"))))

(defn- either-nil?
  [& args]
  (reduce (fn [memo arg]
            (or memo (nil? arg)))
          false
          args))

(defn- from-top-or-left
  [trees]
  (->> trees
       (reduce (fn [[highest-trees-north visibility] line]
                 (loop [[tree & line-rest] line
                        [highest-tree-north & highest-trees-north-rest] highest-trees-north
                        highest-tree-west -1
                        highest-trees '()
                        visibility-line '()]
                   (if (either-nil? tree highest-tree-north)
                     [(reverse highest-trees)
                      (cons (reverse visibility-line) visibility)]
                     (recur line-rest
                            highest-trees-north-rest
                            (max highest-tree-west tree)
                            (cons (max tree highest-tree-north)
                                  highest-trees)
                            (cons (or (> tree highest-tree-west)
                                      (> tree highest-tree-north))
                                  visibility-line)))))
               [(-> trees
                    first
                    count
                    (repeat -1))
                '()])
       second
       reverse))

(defn- flip
  [trees]
  (->> trees
       (map reverse)
       reverse))

(defn- from-bottom-or-right
  [trees]
  (-> trees
      flip
      from-top-or-left
      flip))

(def first-result
  (loop [[line1 & top-left-rest] (from-top-or-left data)
         [line2 & bottom-right-rest] (from-bottom-or-right data)
         total 0]
    (if (either-nil? line1 line2)
      total
      (recur top-left-rest
             bottom-right-rest
             (loop [[x & rest1] line1
                    [y & rest2] line2
                    subtotal total]
               (if (either-nil? x y)
                 subtotal
                 (recur rest1
                        rest2
                        (if (or x y)
                          (inc subtotal)
                          subtotal)))))))) 

(defn- look-right
  [trees]
  (map (fn [line]
         (loop [result '()
                [tree & trees-rightward] line]
           (if (empty? trees-rightward)
             (reverse (cons 0 result))
             (let [trees-split (split-with (partial > tree)
                                          trees-rightward)]
               (recur (cons (+ (count (first trees-split))
                               (if (empty? (last trees-split)) 0 1))
                            result)
                      trees-rightward)))))
       trees))

(defn- look-down
  [trees]
  (loop [result '()
         [line & trees-downward] trees]
    (if (empty? trees-downward)
      (reverse (cons (repeat (count line) 0) result))
      (recur (cons (loop [result2 (repeat (count line) 0)
                          [line2 & trees-downward2] trees-downward
                          index 1]
                     (if (empty? trees-downward2)
                       (map (fn [tree]
                              (if (zero? tree) index tree))
                            result2)
                       (recur (loop [result3 '()
                                     [tree & trees-rightward] result2
                                     [tree1 & trees-rightward1] line
                                     [tree2 & trees-rightward2] line2]
                                (let [result4 (cons (if (and (zero? tree)
                                                             (>= tree2 tree1))
                                                      index
                                                      tree)
                                                    result3)]
                                  (if (empty? trees-rightward)
                                    (reverse result4)
                                    (recur result4
                                           trees-rightward
                                           trees-rightward1
                                           trees-rightward2))))
                              trees-downward2
                              (inc index))))
                   result)
             trees-downward))))

(defn- multiply
  [trees1 trees2]
  (loop [result '()
         [line1 & trees-downward1] trees1
         [line2 & trees-downward2] trees2]
    (let [running-result (cons (loop [line-result '()
                                      [tree1 & trees-rightward1] line1
                                      [tree2 & trees-rightward2] line2]
                                 (let [running-line-result (cons (* tree1 tree2)
                                                                 line-result)]
                                   (if (or (empty? trees-rightward1)
                                           (empty? trees-rightward2))
                                     (reverse running-line-result)
                                     (recur running-line-result
                                            trees-rightward1
                                            trees-rightward2))))
                               result)]
      (if (or (empty? trees-downward1)
              (empty? trees-downward2))
        (reverse running-result)
        (recur running-result
               trees-downward1
               trees-downward2)))))

(defn- look-right-and-down
  [trees]
  (multiply (look-right trees) (look-down trees)))

(defn- look-left-and-up
  [trees]
  (flip (look-right-and-down (flip trees))))

(def second-result
  (apply max (map (fn [line] (apply max line))
                  (multiply (look-right-and-down data)
                            (look-left-and-up data)))))
