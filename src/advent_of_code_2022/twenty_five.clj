(ns advent-of-code-2022.twenty_five
  (:require [clojure.string :as str]))

(def ^:private data
  (->> "resources/twenty_five.txt"
       slurp
       str/split-lines))

(defn- snafu-to-decimal [snafu]
  (reduce (fn [value char]
            (+ (* value 5)
               (case char
                 \= -2
                 \- -1
                 \0 0
                 \1 1
                 \2 2)))
          0
          snafu))

(defn- decimal-to-snafu [decimal]
  (loop [value decimal
         snafu ""]
    (if (zero? value)
      snafu
      (let [q (quot value 5)
            r (rem value 5)
            v (if (> r 2) (inc q) q)
            s (case r 3 \= 4 \- 0 \0 1 \1 2 \2)]
        (recur v (str/join [s snafu]))))))

(def first-result
  (->> data
       (reduce (fn [total line]
                 (+ total (snafu-to-decimal line)))
               0)
       decimal-to-snafu))
