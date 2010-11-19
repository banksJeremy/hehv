(ns hehvolution.core
    "Simulation!!!"
    (:use hehvolution.general))

(def mutation-stdev 0.1)

(def genes [:unhunger :avoidance :life-to-repo])
(def guy-speed 0.1)

(defn geneome
  "Generates a geneome, uniformly random or from mutating an existing one."
  ([]
    (apply hash-map (flatten (for [gene genes] [gene (rand)]))))
  ([parent]
    parent)) ; TODO

(defn point
  [x y]
    {:x x :y y})

(defn point-sum
  [a b]
    (point (+ (a :x) (b :x)) (+ (a :y) (b :y))))

(defn point-sub
  [a b]
    (point (- (a :x) (b :x)) (- (a :y) (b :y))))

(defn point-scale
  [p scale]
    (point (* scale (p :x)) (* scale (p :y))))

(defn point-mag
  [p]
    (Math/pow (+ (Math/pow (p :x) 2) (Math/pow (p :y) 2)) 0.5))

(defn point-direction
  [p]
    (if (== (point-mag p) 0)
        p
        (point-scale p (/ 1 (point-mag p)))))

(defn guy
  "Creates a new guy."
  ([]
    (guy 1.0 1.0))
  ([sim-width sim-height]
    {:loc (point (* (rand) sim-width) (* (rand) sim-height))
     :life 1.0
     :geneome (geneome)
     :radius 6
     :type :guy}))

(defmulti influence-on-guy
  "How much a guy is influenced by something."
  :type)

(defmethod influence-on-guy :guy
  [other self]
  (if (or (identical? other self) (<= (other :life) 0))
      (point 0 0)
      (let [delta (point-sub (self :loc) (other :loc))]
        (point-scale delta (/ ((self :geneome) :avoidance) (Math/pow (point-mag delta) 2))))))

(defmethod influence-on-guy :resource
  [other self]
  (if (<= (other :remaining) 0)
      (point 0 0)
      (let [delta (point-sub (other :loc) (self :loc))]
        (point-scale delta (/ ((self :geneome) :unhunger) (Math/pow (point-mag delta) 2))))))

(defn guy-direction
  "Returns the direction a guy would like to move in, given a state."
  [self state]
    (point-direction
      (reduce point-sum (point 0 0) 
              (map #(influence-on-guy % self) (concat (state :guys) (state :resources))))))

(defn guy-velocity
  "Returns the velocity a guy would move at, given a state."
  [self state]
    (point-scale (guy-direction self state) guy-speed))

(defn resource
  ([]
    (resource 1.0 1.0))
  ([sim-width sim-height]
    {:loc (point (* (rand) sim-width) (* (rand) sim-height))
     :remaining (+ 0.5 (rand))
     :radius 3
     :type :resource}))

(defn colliding?
  [a b]
    (<= (point-mag (point-sub a b)) (+ (a :radius) (b :radius))))

(defn simulation
  "Yeah!"
  ([]
    (simulation 10 20 120 120))
  ([n-guys n-resources width height]
    (let [initial-state {
            :guys (for [_ (range n-guys)] (guy width height))
            :resources (for [_ (range n-resources)] (resource width height))}]
      {:initial-state initial-state
       :state (ref initial-state)
       :width width
       :height height})))

(defn advanced-state
  "Returns what state becomes after a generation."
  [state]
    (assoc state
      ; map actions over guys
      ; map actions over resources
      ; filter nil out of both lists
      ; make both lists the same one? :types distinguish.
      :guys (map #(assoc % :loc (point-sum (% :loc) (guy-velocity % state))
                           :life (- (% :life) 0.001)) 
                 (state :guys))))

(defn tick-sim
  "Tick."
  [sim]
    (dosync (alter (sim :state) advanced-state)))

(defn sim-frequently-tick
  "Runs a simulation in a seperate thread."
  [sim hertz alive-ref]
    (thread-running (fn []
      (if @alive-ref (do
        (Thread/sleep (* 1000 (/ 1 hertz)))
        (tick-sim sim)
        (recur))))))
