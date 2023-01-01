(ns advent-of-code-2022.twenty_four
  (:require [clojure.string :as str]))

(defn- process-space
  [state [x y] direction]
  (case direction
    \. (let [start (state :start)]
         [(assoc state
                 :start (if (nil? start) [x y] start)
                 :end [x y])
          (inc x)])
    \# (let [[west-x north-y east-x south-y] (state :extent)]
         [(assoc state
                 :extent [(min x west-x)
                          (min y north-y)
                          (max x east-x)
                          (max y south-y)])
          (inc x)])
    (let [blizzards (state :blizzards)]
      [(assoc state
              :blizzards (assoc blizzards [x y] (list direction)))
       (inc x)])))

(defn- load-data [filename]
  (->> filename
       slurp
       str/split-lines
       (reduce (fn [[memo y] line]
                 (->> line
                      (reduce (fn [[memo-inner x] char]
                                (process-space memo-inner [x y] char))
                              [memo 0])
                      ((fn [[updated _]] [updated (inc y)]))))
               [{:blizzards {} :extent [0 0 0 0] :start nil :end nil} 0])
       ((fn [[{[start-x start-y] :start [end-x end-y] :end :as state} _]]
          (assoc state
                 :start [start-x (inc start-y)]
                 :end [end-x (dec end-y)])))))

(def test-data (load-data "resources/twenty_four_test.txt"))

(def data (load-data "resources/twenty_four.txt"))

(defn- move-blizzard
  [[x y] direction [west-x north-y east-x south-y]]
  (case direction
    \^ (let [Y (dec y)]
         [x (if (= Y north-y) (dec south-y) Y)])
    \v (let [Y (inc y)]
         [x (if (= Y south-y) (inc north-y) Y)])
    \< (let [X (dec x)]
         [(if (= X west-x) (dec east-x) X) y])
    \> (let [X (inc x)]
         [(if (= X east-x) (inc west-x) X) y])))

(defn- move-blizzards
  [{blizzards :blizzards extent :extent :as state}]
  (assoc state
         :blizzards (reduce (fn [memo [coord directions]]
                              (reduce (fn [memo-inner direction]
                                        (let [new-coord (move-blizzard coord
                                                                       direction
                                                                       extent)]
                                          (if-some [entry (memo-inner new-coord)]
                                            (assoc memo-inner
                                                   new-coord (cons direction entry))
                                            (assoc memo-inner
                                                   new-coord (list direction)))))
                                      memo
                                      directions))
                            {}
                            blizzards)))

(defn- possible-moves
  [{blizzards :blizzards
    [west-x north-y east-x south-y] :extent
    start :start}
   coord]
  (if (nil? coord)
    (if (contains? blizzards start)
      '(nil)
      (list start nil))
    (let [[x y] coord
          x-west (dec x)
          x-east (inc x)
          y-north (dec y)
          y-south (inc y)]
      (concat
       (if (or (= x-west west-x)
               (contains? blizzards
                          [x-west y]))
         '()
         (list [x-west y]))
       (if (or (= x-east east-x)
               (contains? blizzards
                          [x-east y]))
         '()
         (list [x-east y]))
       (if (or (= y-north north-y)
               (contains? blizzards
                          [x y-north]))
         '()
         (list [x y-north]))
       (if (or (= y-south south-y)
               (contains? blizzards
                          [x y-south]))
         '()
         (list [x y-south]))
       (if (contains? blizzards coord)
         '()
         (list coord))
       ))))

(defn- find-least-moves [initial-minutes initial-state]
  (loop [{end :end :as state} initial-state
         positions #{nil}
         minutes initial-minutes]
    (if (contains? positions end)
      [minutes state]
      (let [updated-state (doall (move-blizzards state))]
        (recur updated-state
               (into #{} (mapcat (partial possible-moves
                                          updated-state)
                                 positions))
               (inc minutes))))))

(def first-test
  (->> test-data
       (find-least-moves 1)
       first))

(def first-result 
  (->> data
       (find-least-moves 1)
       first))

(defn- swap-start-and-end
  [[minutes {start :start end :end :as state}]]
  [minutes
   (assoc state
          :start end
          :end start)])

(def second-result
  (->> data
       (find-least-moves 1)
       swap-start-and-end
       (apply find-least-moves)
       swap-start-and-end
       (apply find-least-moves)
       first))
