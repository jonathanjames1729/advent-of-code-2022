(ns advent-of-code-2022.seven
  (:require [clojure.string :as str]))

(defn- parse-line
  [line]
  (re-find #"^(?:(?:\$\s*(?:(?:cd\s+(\S+))|(?:ls)))|(?:dir\s+(\S+))|(?:(\d+)\s+(\S+)))\s*$" 
           line))

(defn- string-path
  [dir-path base-path]
  (str/join "/" (reverse (cons base-path dir-path))))

(def ^:private data
  (get (reduce (fn [[path dirs] line]
                 (let [[_ component dir size file] (parse-line line)]
                   (if (some? component)
                     (case component
                       "/" ['("") dirs]
                       ".." [(rest path) dirs]
                       [(cons component path) dirs])
                     (if (some? dir)
                       [path (cons [(string-path path dir) 0] dirs)]
                       (if (some? file)
                         [path
                          (let [file-path (string-path path file)]
                            (map (fn [[dir-path dir-size]]
                                   (if (str/starts-with? file-path dir-path)
                                     [dir-path (+ dir-size (Integer/parseInt size))]
                                     [dir-path dir-size]))
                                 dirs))]
                         [path dirs])))))
               ['("") '(["/" 0])]
               (str/split-lines (slurp "resources/seven.txt"))) 1))

(def first-result
  (reduce + (filter (partial >= 100000) (map second data))))

(def second-result
  (let [space-to-free (- (second (last data)) 40000000)]
    (second (first (sort (fn [[_ size1] [_ size2]]
                           (< size1 size2))
                         (filter (fn [[_ size]]
                                   (>= size space-to-free))
                                 data))))))
