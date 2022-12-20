(ns advent-of-code-2022.seventeen
  (:require [clojure.string :as str]))

(def data ^:private
  (-> "resources/seventeen.txt"
      slurp
      (str/split #"")))

(def first-result 0)

(def second-result 0)
