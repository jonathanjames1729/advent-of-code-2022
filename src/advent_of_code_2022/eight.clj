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

(def second-result 0)
