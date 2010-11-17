(ns hehvolution.core)

(def mutation-stdev 0.1)

(def genes [:hunger :avoidance :life-to-repo])
(def guy-speed 0.1)

(defn geneotype
  "Generates a genotype, uniformly random or from mutating an existing one."
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
     :geneotype (geneotype)
     :type :guy}))

(defmulti guy-influence-on
  "How much a guy is influenced by something."
  :type)

(defmethod guy-influence-on :guy
  [other self]
  (if (or (identical? other self) (<= (other :life) 0))
      (point 0 0)
      (point-scale (point-sub (self :loc) (other :loc))
                   ((self :geneotype) :avoidance))))

(defn guy-direction
  "Returns the direction a guy would like to move in, given a state."
  [self state]
    (point-direction
      (reduce point-sum (point 0 0) 
              (map #(guy-influence-on % self) (state :guys)))))

(defn guy-velocity
  "Returns the velocity a guy would move at, given a state."
  [self state]
    (point-scale (guy-direction self state) guy-speed))

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
    (assoc state
      :guys (map #(assoc % :loc (point-sum (% :loc) (guy-velocity % state))) 
                 (state :guys))))

(defn tick-sim
  "Tick."
  [sim]
  (dosync (alter (sim :state) advanced-state)))

(defn GOGOGO []
  "do something to see if it blows up"
  (tick-sim (simulation)))


