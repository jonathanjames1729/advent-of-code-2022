(ns advent-of-code-2022.twenty_one
  (:require [clojure.string :as str]))

(def ^:private pattern
  (re-pattern (str/join ["^\\s*([a-z][a-z][a-z][a-z]):\\s+"
                         "(?:(\\d+)|"
                         "(?:([a-z][a-z][a-z][a-z])\\s+"
                         "([-+*/])\\s+"
                         "([a-z][a-z][a-z][a-z])))\\s*$"])))

(def ^:private data
  (->> "resources/twenty_one.txt"
       slurp
       str/split-lines
       (map (fn [line] (rest (re-find pattern line))))
       (reduce (fn [memo [key result one operand two]]
                 (assoc memo
                        key {:result (if (nil? result) nil (Integer/parseInt result))
                             :operand (case operand "+" + "-" - "*" * "/" / nil)
                             :one one
                             :two two}))
               {})))

(defn- resolve-monkeys [monkeys]
  (loop [state monkeys]
    (if (some? ((state "root") :result))
      ((state "root") :result)
      (recur (into {}
                   (map (fn [[key value]]
                          (if (some? (value :result))
                            [key value]
                            (let [one ((get state (value :one)) :result)
                                  two ((get state (value :two)) :result)]
                              (if (or (nil? one) (nil? two))
                                [key value]
                                [key (assoc value :result ((value :operand) one two))]))))
                        state))))))

(def first-result (resolve-monkeys data))

(defn- evaluate-monkeys [human]
  (resolve-monkeys (assoc data
                          "root" (assoc (data "root") 
                                        :operand -)
                          "humn" (assoc (data "humn")
                                        :result human))))

(def second-result
  (let [b (evaluate-monkeys 0)
        a (- (evaluate-monkeys 1) b)
        x (/ (- b) a)]
    (if (zero? (evaluate-monkeys x)) x nil)))
