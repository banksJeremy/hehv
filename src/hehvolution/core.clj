(ns hehvolution.core)

(def mutation-stdev 0.1)

(def genes [:fear :hunger :life-to-repo])

(defn geneotype
  "Generates a genotype, uniformly random or from mutating an existing one."
  ([]
    (apply hash-map (flatten (for [gene genes] [gene (rand)]))))
  ([parent]
    parent)) ; TODO

(defn guy
  "Creates a new guy."
  [sim-width sim-height]
    {:x (* (rand) sim-width)
     :y (* (rand) sim-height)
     :life 1.0
     :geneotype (geneotype)})

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
                                :y (+ (* 2 (rand)) (guy :y) -1)
                                :life (- (guy :life) (* 0.01 (rand)))))
           (state :guys))))

(defn tick-sim
  "Tick."
  [sim]
  (dosync (alter (sim :state) advanced-state)))
