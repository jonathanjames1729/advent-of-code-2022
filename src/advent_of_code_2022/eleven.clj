(ns advent-of-code-2022.eleven
  (:require [clojure.string :as str]))

(def ^:private pattern
  (->> '("Monkey\\s+(\\d+):\\s*"
         "\\s*Starting items:\\s+((?:\\d+,\\s*)*\\d+)\\s*"
         "\\s*Operation:\\s*new\\s*=\\s*old\\s*(\\+|\\*)\\s*((?:old)|(?:\\d+))\\s*"
         "\\s*Test:\\s+divisible by\\s+(\\d+)\\s*"
         "\\s*If true:\\s+throw to monkey\\s+(\\d+)\\s*"
         "\\s*If false:\\s+throw to monkey\\s+(\\d+)\\s*")
       (str/join "\n")
       re-pattern))

(def ^:private data
  (->> "resources/eleven.txt"
       slurp
       (re-seq pattern)
       (map (fn [monkey]
              {:items (map (fn [x] (Integer/parseInt x))
                           (str/split (monkey 2) #"\s*,\s*"))
               :operation [(monkey 3)
                           (let [op (monkey 4)]
                             (if (= op "old")
                               nil
                               (Integer/parseInt op)))]
               :test (vec (map (fn [i]
                                 (Integer/parseInt (get monkey i)))
                               '(5 6 7)))
               :inspections 0}))
       vec))

(def ^:private monkey-count (count data))

(defn- transform-item-worries
  [monkey decreaser]
  (map (fn [item]
         (let [op (get-in monkey [:operation 0])
               arg (if-let [op-arg (get-in monkey [:operation 1])]
                     op-arg
                     item)]
           (decreaser (case op
                        "+" (+ item arg)
                        "*" (* item arg)))))
       (monkey :items)))

(defn- test-items
  [monkey items]
  (let [test (get-in monkey [:test 0])]
    (->> items
         (reduce (fn [memo item]
                   (if (zero? (mod item test))
                     [(cons item (memo 0)) (memo 1)]
                     [(memo 0) (cons item (memo 1))]))
                 ['() '()])
         (map reverse))))

(defn- throw-items
  [monkey decreaser]
  (test-items monkey
              (transform-item-worries monkey decreaser)))

(defn- do-monkey-business
  [rounds decreaser]
  (->> (range (* rounds monkey-count))
       (reduce (fn [memo index]
                 (let [monkey-index (mod index monkey-count)
                       monkey (memo monkey-index)
                       [true-items false-items] (throw-items monkey
                                                             decreaser)
                       true-monkey-index (get-in monkey [:test 1])
                       false-monkey-index (get-in monkey [:test 2])]
                   (-> memo
                       (assoc monkey-index
                              (assoc monkey
                                     :items '()
                                     :inspections (+ (monkey :inspections)
                                                     (count true-items)
                                                     (count false-items))))
                       (assoc-in [true-monkey-index :items]
                                 (concat (get-in memo
                                                 [true-monkey-index :items])
                                         true-items))
                       (assoc-in [false-monkey-index :items]
                                 (concat (get-in memo
                                                 [false-monkey-index :items])
                                         false-items)))))
               data)
       (map (fn [monkey] (monkey :inspections)))
       (sort >)
       ((fn [inspections] (* (first inspections) (second inspections))))))

(def first-result (do-monkey-business 20 (fn [x] (quot x 3))))

(def ^:private multiple
  (reduce (fn [result monkey]
            (* result (get-in monkey [:test 0])))
          1
          data))

(def second-result (do-monkey-business 10000
                                       (fn [x] (mod x multiple))))
