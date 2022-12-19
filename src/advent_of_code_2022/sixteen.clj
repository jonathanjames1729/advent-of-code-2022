(ns advent-of-code-2022.sixteen
  (:require [clojure.string :as str]))

(def ^:private pattern
  (re-pattern
   (str/join (list "^Valve ([A-Z][A-Z]) has flow rate=(\\d+);\\s+"
                   "tunnels? leads? to valves?\\s+"
                   "([A-Z][A-Z](?:,\\s+[A-Z][A-Z])*)\\s*$"))))

(def ^:private data
  (->> "resources/sixteen.txt"
       slurp
       str/split-lines
       (mapcat (fn [line]
                 (->> line
                      (re-find pattern)
                      ((fn [[_ valve flow valves]]
                         (list valve
                               {:flow (Integer/parseInt flow)
                                :valves (str/split valves #",\s+")}))))))
       (apply sorted-map)))

(def ^:private valves-to-open-internal
  (reduce (fn [[valves-to-open max-flow] [valve detail]]
            (let [flow (detail :flow)]
              (if (pos? flow)
                [(conj valves-to-open valve) (+ max-flow flow)]
                [valves-to-open max-flow])))
          [#{} 0]
          data))

(def ^:private valves-to-open
  (valves-to-open-internal 0))

(def ^:private max-flow
  (valves-to-open-internal 1))

(defn- no-more-moves-nessesary? [state]
  (= (state :open-valves) valves-to-open))

(defn- updated-total-flow [total-flow open-valves]
  (reduce (fn [memo valve]
            (+ memo (get-in data [valve :flow])))
          total-flow
          open-valves))

(defn- valve-can-not-be-opened? [valve open-valves]
  (or (contains? open-valves valve)
      (zero? (get-in data [valve :flow]))))

(defn- possible-states-after-a-minute-1
  [{current-valves :current-valves
    open-valves :open-valves
    total-flow :total-flow}]
  (let [current-valve (first current-valves)
        valves (get-in data [current-valve :valves])
        updated-flow (updated-total-flow total-flow
                                         open-valves)
        updated-states (map (fn [valve]
                              {:current-valves #{valve}
                               :open-valves open-valves
                               :total-flow updated-flow})
                            valves)]
    (if (valve-can-not-be-opened? current-valve open-valves)
      updated-states
      (cons {:current-valves current-valves
             :open-valves (conj open-valves
                                current-valve)
             :total-flow updated-flow}
            updated-states))))

(defn- possible-states-after-a-minute-2
  [{current-valves :current-valves
    open-valves :open-valves
    total-flow :total-flow}]
  (let [valves (map (fn [current-valve]
                      (get-in data [current-valve :valves]))
                    current-valves)
        updated-flow (updated-total-flow total-flow
                                         open-valves)
        current-valve-1 (first current-valves)
        valves-1 (first valves)]
    (if (> (count current-valves) 1)
      (let [current-valve-2 (second current-valves)
            valves-2 (second valves)]
        (mapcat (fn [valve-1]
                  (map (fn [valve-2]
                         {:current-valves (conj #{(if (nil? valve-1)
                                                    current-valve-1
                                                    valve-1)}
                                                (if (nil? valve-2)
                                                  current-valve-2
                                                  valve-2))
                          :open-valves (into open-valves
                                             (concat (if (nil? valve-1)
                                                       (list current-valve-1) '())
                                                     (if (nil? valve-2)
                                                       (list current-valve-2) '())))
                          :total-flow updated-flow})
                       (if (valve-can-not-be-opened? current-valve-2 open-valves)
                         valves-2
                         (cons nil valves-2))))
                (if (valve-can-not-be-opened? current-valve-1 open-valves)
                  valves-1
                  (cons nil valves-1))))
      (mapcat (fn [valve-1]
                (map (fn [valve-2]
                       {:current-valves (conj #{(if (nil? valve-1)
                                                  current-valve-1
                                                  valve-1)}
                                              valve-2)
                        :open-valves (if (nil? valve-1)
                                       (conj open-valves current-valve-1)
                                       open-valves)
                        :total-flow updated-flow})
                     valves-1))
              (if (valve-can-not-be-opened? current-valve-1 open-valves)
                valves-1
                (cons nil valves-1))))))

(defn- possible-states-after-a-minute [state workers]
  (case workers
    1 (possible-states-after-a-minute-1 state)
    2 (possible-states-after-a-minute-2 state)))

(defn- make-all-next-moves [states workers]
  (mapcat (fn [state]
            (if (no-more-moves-nessesary? state)
              (assoc state
                     :total-flow (+ (state :total-flow)
                                    max-flow))
              (possible-states-after-a-minute state workers)))
          states))

(defn- prune-states [states]
  (->> states
       (reduce (fn [memo
                    {current-valves :current-valves
                     open-valves :open-valves
                     total-flow :total-flow}]
                 (let [current-valves-key (str/join current-valves)]
                   (if (contains? memo current-valves-key)
                     (let [{current-open-valves :open-valves
                            current-total-flow :total-flow} (get memo
                                                                 current-valves-key)]
                       (cond
                         (> total-flow current-total-flow)
                         (assoc memo
                                current-valves-key
                                {:open-valves #{open-valves}
                                 :total-flow total-flow})
                         (= total-flow current-total-flow)
                         (assoc memo
                                current-valves-key
                                {:open-valves (conj current-open-valves
                                                    open-valves)
                                 :total-flow total-flow})
                         :else memo))
                     (assoc memo
                            current-valves-key
                            {:open-valves #{open-valves}
                             :total-flow total-flow}))))
               (sorted-map))
       (mapcat (fn [[current-valves
                     {open-valves-set :open-valves
                      total-flow :total-flow}]]
                 (map (fn [open-valves]
                        {:current-valves (set (re-seq #"[A-Z][A-Z]"
                                                      current-valves))
                         :open-valves open-valves
                         :total-flow total-flow})
                      open-valves-set)))))

(def first-result
  (->> (range 0 30)
       (reduce (fn [states _]
                 (-> states
                     (make-all-next-moves 1)
                     prune-states))
               (list {:current-valves #{"AA"}
                      :open-valves #{}
                      :total-flow 0}))
       (map (fn [state] (state :total-flow)))
       (apply max)))

(def second-result
  (->> (range 0 26)
       (reduce (fn [states _]
                 (-> states
                     (make-all-next-moves 2)
                     prune-states))
               (list {:current-valves #{"AA"}
                      :open-valves #{}
                      :total-flow 0}))
        (map (fn [state] (state :total-flow)))
        (apply max)))
