(ns advent-of-code-2022.twenty_three
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn- load-data [filename]
  (->> filename
       slurp
       str/split-lines
       (reduce (fn [[elves y] line]
                 [(->> line
                       (reduce (fn [[elves-sub x] char]
                                 [(if (= char \#)
                                    (conj elves-sub [x y])
                                    elves-sub)
                                  (inc x)])
                               [elves 0])
                       first)
                  (inc y)])
               [#{} 0])
       first))

(def ^:private test-data
  (load-data "resources/twenty_three_test.txt"))

(def ^:private data
  (load-data "resources/twenty_three.txt"))

(defn- north? [elves [x y]]
  (if (empty? (set/intersection elves
                                #{[(dec x) (dec y)]
                                  [x (dec y)]
                                  [(inc x) (dec y)]}))
    [x (dec y)]
    nil))

(defn- south? [elves [x y]]
  (if (empty? (set/intersection elves
                                #{[(dec x) (inc y)]
                                  [x (inc y)]
                                  [(inc x) (inc y)]}))
    [x (inc y)]
    nil))

(defn- west? [elves [x y]]
  (if (empty? (set/intersection elves
                                #{[(dec x) (dec y)]
                                  [(dec x) y]
                                  [(dec x) (inc y)]}))
    [(dec x) y]
    nil))

(defn- east? [elves [x y]]
  (if (empty? (set/intersection elves
                             #{[(inc x) (dec y)]
                               [(inc x) y]
                               [(inc x) (inc y)]}))
    [(inc x) y]
    nil))

(def ^:private directions [north? south? west? east?])

(defn- alone? [elves coord]
  (reduce (fn [result direction]
            (and (direction elves coord)
                 result))
          true
          directions))

(defn- propose-move [elves round coord]
  (if (alone? elves coord)
    coord
    (->> (range 5)
         (map (fn [i]
                (if (< i 4)
                  ((get directions
                        (mod (+ round i) 4)) elves coord)
                  coord)))
         (some identity))))

(defn- make-moves [elves round]
  (->> elves
       (reduce (fn [proposed coord]
                 (let [proposed-coord (propose-move elves
                                                    round
                                                    coord)]
                   (if (contains? proposed proposed-coord)
                     (if-some [last-coord (get proposed proposed-coord)]
                       (assoc proposed
                              coord coord
                              last-coord last-coord
                              proposed-coord nil)
                       (assoc proposed
                              coord coord))
                     (assoc proposed
                            proposed-coord coord))))
               {})
       (reduce (fn [proposed [proposed-coord coord]]
                 (if (nil? coord) proposed (conj proposed proposed-coord)))
               #{})))

(defn- minimal-extent [elves]
  (reduce (fn [[min-x min-y max-x max-y] [x y]]
            (if (nil? min-x)
              [x y x y]
              [(min min-x x) (min min-y y) (max max-x x) (max max-y y)]))
          [nil nil nil nil]
          elves))

(defn- rectangle-area [[min-x min-y max-x max-y]]
  (* (inc (- max-x min-x))
     (inc (- max-y min-y))))

(defn- empty-tiles [elves]
  (->> elves
       minimal-extent
       rectangle-area
       ((fn [area] (- area (count elves))))))

(defn- display [elves]
  (let [[min-x min-y max-x max-y] (minimal-extent elves)]
    (str/join "\n"
              (map (fn [y]
                     (str/join (map (fn [x]
                                      (if (contains? elves [x y]) \# \.))
                                    (range min-x (inc max-x)))))
                   (range min-y (inc max-y))))))

(def first-test
  (->> (range 10)
       (reduce (fn [[s elves] round]
                 (let [updated (make-moves elves round)]
                   [(str/join "\n" [s
                                    ""
                                    (format "== End of Round %d ==" (inc round))
                                    (display updated)])
                    updated]))
               [(str/join "\n" ["== Initial State =="
                                (display test-data)])
                test-data])
       ((fn [[s elves]]
          (str/join "\n"
                    [s
                     ""
                     "== Empty Tiles =="
                     (empty-tiles elves)])))))

(def first-result
  (->> (range 10)
       (reduce make-moves data)
       empty-tiles))

(def second-result
  (inc (loop [elves data
              round 0]
         (let [updated (make-moves elves round)]
           (if (= updated elves)
             round
             (recur updated (inc round)))))))
