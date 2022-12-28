(ns advent-of-code-2022.nineteen
  (:require [clojure.string :as str]))

(def ^:private pattern
  (re-pattern
   (str/join
    (list
     "^Blueprint (\\d+):\\s+"
     "Each ore robot costs (\\d+) ore.\\s+"
     "Each clay robot costs (\\d+) ore.\\s+"
     "Each obsidian robot costs (\\d+) ore and (\\d+) clay.\\s+"
     "Each geode robot costs (\\d+) ore and (\\d+) obsidian.$"))))

(def data ^:private
  (->> "resources/nineteen.txt"
       slurp
       str/split-lines
       (map (fn [line]
              (->> line
                   (re-find pattern)
                   rest
                   (map (fn [n] (Integer/parseInt n))))))))

(defn- max-geodes-opened
  [minutes
   [id-number ore-cost-ore clay-cost-ore
    obsidian-cost-ore obsidian-cost-clay
    geode-cost-ore geode-cost-obsidian]]
  (->> (range 0 (- minutes 2))
       (reduce (fn [states index]
                 (->> states
                      (reduce (fn [[memo max-estimated-min] state]
                                (let [{ore :ore ore-robots :ore-robots
                                       may-build-ore-robot :may-build-ore-robot
                                       clay :clay clay-robots :clay-robots
                                       may-build-clay-robot :may-build-clay-robot
                                       obsidian :obsidian
                                       obsidian-robots :obsidian-robots
                                       may-build-obsidian-robot :may-build-obsidian-robot
                                       geodes :geodes geode-robots :geode-robots
                                       may-build-geode-robot :may-build-geode-robot} state
                                      ore-updated (+ ore ore-robots)
                                      clay-updated (+ clay clay-robots)
                                      obsidian-updated (+ obsidian
                                                          obsidian-robots)
                                      geodes-updated (+ geodes
                                                        geode-robots)
                                      can-build-geode-robot (and (>= ore geode-cost-ore)
                                                                 (>= obsidian
                                                                     geode-cost-obsidian))
                                      can-build-obsidian-robot (and (>= ore
                                                                        obsidian-cost-ore)
                                                                    (>= clay
                                                                        obsidian-cost-clay))
                                      can-build-clay-robot (>= ore clay-cost-ore)
                                      can-build-ore-robot (>= ore ore-cost-ore)
                                      estimated-min (if can-build-geode-robot
                                                      (+ geodes-updated
                                                         (* (- minutes index 1)
                                                            (inc geode-robots)))
                                                      (+ geodes
                                                         (* (- minutes index)
                                                            geode-robots)))
                                      estimated-max-large (+ geodes (/ (* (- minutes index)
                                                                          (- (+ (* 2 geode-robots)
                                                                                minutes)
                                                                             index 1))
                                                                       2))
                                      estimated-max-small (+ geodes-updated
                                                             (/ (* (- minutes index 1)
                                                                   (- (+ (* 2 geode-robots)
                                                                         minutes)
                                                                      index 2))
                                                                2))
                                      state-updated (assoc state
                                                           :ore ore-updated
                                                           :clay clay-updated
                                                           :obsidian obsidian-updated
                                                           :geodes geodes-updated
                                                           :may-build-ore-robot true
                                                           :may-build-clay-robot true
                                                           :may-build-obsidian-robot true
                                                           :may-build-geode-robot true
                                                           :estimated-max estimated-max-small)]
                                  (if (> estimated-max-large max-estimated-min)
                                    [(into memo
                                           (concat
                                            (if (and can-build-geode-robot
                                                     may-build-geode-robot)
                                              (list (assoc state-updated
                                                           :ore (- ore-updated
                                                                   geode-cost-ore)
                                                           :obsidian (- obsidian-updated
                                                                        geode-cost-obsidian)
                                                           :geode-robots (inc geode-robots)
                                                           :estimated-max estimated-max-large))
                                              '())
                                            (if (and can-build-obsidian-robot
                                                     may-build-obsidian-robot
                                                     (> estimated-max-large max-estimated-min))
                                              (list (assoc state-updated
                                                           :ore (- ore-updated
                                                                   obsidian-cost-ore)
                                                           :clay (- clay-updated
                                                                    obsidian-cost-clay)
                                                           :obsidian-robots (inc obsidian-robots)))
                                              '())
                                            (if (and can-build-clay-robot
                                                     may-build-clay-robot
                                                     (> estimated-max-small max-estimated-min))
                                              (list (assoc state-updated
                                                           :ore (- ore-updated
                                                                   clay-cost-ore)
                                                           :clay-robots (inc clay-robots)))
                                              '())
                                            (if (and can-build-ore-robot
                                                     may-build-ore-robot
                                                     (> estimated-max-small max-estimated-min))
                                              (list (assoc state-updated
                                                           :ore (- ore-updated
                                                                   ore-cost-ore)
                                                           :ore-robots (inc ore-robots)))
                                              '())
                                            (if (and can-build-geode-robot
                                                     can-build-obsidian-robot
                                                     can-build-clay-robot
                                                     can-build-ore-robot
                                                     (> estimated-max-small max-estimated-min))
                                              '()
                                              (list (assoc state-updated
                                                           :may-build-ore-robot
                                                           (not can-build-ore-robot)
                                                           :may-build-clay-robot
                                                           (not can-build-clay-robot)
                                                           :may-build-obsidian-robot
                                                           (not can-build-obsidian-robot)
                                                           :may-build-geode-robot
                                                           (not can-build-geode-robot))))))
                                     (max max-estimated-min estimated-min)]
                                    [memo (max max-estimated-min estimated-min)])))
                              [#{} 0])
                      ((fn [[states-updated max-estimated-min]]
                         (filter (fn [state]
                                   (> (state :estimated-max)
                                      max-estimated-min))
                                 states-updated)))
                      set))
               #{{:ore 0 :clay 0 :obsidian 0 :geodes 0
                  :ore-robots 1 :clay-robots 0
                  :obsidian-robots 0 :geode-robots 0
                  :may-build-ore-robot true
                  :may-build-clay-robot true
                  :may-build-obsidian-robot true
                  :may-build-geode-robot true
                  :estimated-min 0
                  :estimated-max 0}})
       (reduce (fn [max-geodes state]
                 (let [geodes (+ (state :geodes)
                                 (* 2 (state :geode-robots))
                                 (if (and (>= (state :ore)
                                              geode-cost-ore)
                                          (>= (state :obsidian)
                                              geode-cost-obsidian))
                                   1 0))]
                   (if (> geodes max-geodes) geodes max-geodes)))
               0)
       ((fn [max-geodes] [id-number max-geodes]))))
  
(def first-result
  (->> data
       (pmap (partial max-geodes-opened 24))
       (reduce (fn [total [id-number max-geodes]]
                 (+ total (* id-number max-geodes)))
               0)))

(def second-result
  (->> data
       (take 3)
       (map (partial max-geodes-opened 32))
       (reduce (fn [total [_ max-geodes]] (* total max-geodes))
               1)))
