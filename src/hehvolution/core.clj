(ns hehvolution.core)

(defmacro def-
  "Defines a private "
  [symbol value]
  `(def ^{:private true} ~symbol ~value)); TODO: not ignore existing meta

(defn guy
  "Creates a new guy."
  [sim-width sim-height]
    {:x (* (rand) sim-width) :y (* (rand) sim-height)})

(defn simulation
  "Yeah!"
  ([]
    (simulation 10 120 120))
  ([n-guys width height]
    (let [initial-state {:guys (for [_ (range n-guys)] (guy width height))}]
      {:initial-state initial-state
       :state (ref initial-state)
       :width width
       :height height})))

(defn advanced-state
  "Returns what state becomes after a generation."
  [state]
    (assoc state :guys
      (map (fn [guy] (assoc guy :x (+ (* 2 (rand)) (guy :x) -1)
                                :y (+ (* 2 (rand)) (guy :y) -1)))
           (state :guys))))

(defn tick-sim
  "Tick."
  [sim]
  (dosync (alter (sim :state) advanced-state)))
