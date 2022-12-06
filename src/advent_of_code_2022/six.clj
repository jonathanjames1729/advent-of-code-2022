(ns advent-of-code-2022.six)

(def ^:private data (slurp "resources/six.txt"))

(defn- find-distinct-runs [n]
  (loop [index n
         prefix (take n data)
         remains (drop n data)]
    (if (= (count (set prefix)) n)
      index
      (recur (inc index)
             (concat (rest prefix) (take 1 remains))
             (rest remains)))))

(def first-result (find-distinct-runs 4))

(def second-result (find-distinct-runs 14))
