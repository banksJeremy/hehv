(ns hehvolution.core)

(defmacro def-
  "Defines a private "
  [symbol value]
  `(def ^{:private true} ~symbol ~value)); TODO: not ignore existing meta

(defn simulation
  "Yeah!"
  []
  {:state (ref {:guys [
      {:x 10 :y 20}
      {:x 30 :y 90}]})
    :width 120
    :height 120})

(defn advanced-state
  "Returns what state becomes after a generation."
  [state]
    (assoc state :guys
      (map (fn [guy] (assoc guy :x (+ 1 (guy :x))))
           (state :guys))))

(defn tick-sim
  "Tick."
  [sim]
  (dosync (alter (sim :state) advanced-state)))
