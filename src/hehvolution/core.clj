(ns hehvolution.core
    "Simulation!!!"
    (:use hehvolution.general))

(def mutation-stdev 0.1)

(def genes [:unhunger :avoidance :life-to-repo])
(def guy-speed 0.1)
(def guy-decay-rate 0.01)
(def food-regrowth-rate 0.01)

(defn clamp01 [value] (clamp value 0 1))

(defn geneotype
  "Generates a geneotype, uniformly random or from mutating an existing one."
  ([]
    (apply hash-map (flatten (for [gene genes] [gene (rand)]))))
  ([parent]
    parent)) ; TODO

(defn num2
  [x y]
    {:x x :y y})

(defn num2-sum
  [a b]
    (num2 (+ (a :x) (b :x)) (+ (a :y) (b :y))))

(defn num2-sub
  [a b]
    (num2 (- (a :x) (b :x)) (- (a :y) (b :y))))

(defn num2-scale
  [p scale]
    (num2 (* scale (p :x)) (* scale (p :y))))

(defn num2-mag
  [p]
    (Math/pow (+ (Math/pow (p :x) 2) (Math/pow (p :y) 2)) 0.5))

(defn num2-direction
  [p]
    (if (= (num2-mag p) 0)
        p
        (num2-scale p (/ 1 (num2-mag p)))))

(defn num2-at-angle-tau
  [tau]
    (num2
      (Math/cos (* 2 tau))
      (Math/sin (* 2 tau))))

(defn guy
  "Creates a new guy."
  ([]
    (guy 1.0 1.0))
  ([sim-width sim-height]
    {:loc (num2 (* (rand) sim-width) (* (rand) sim-height))
     :life 0.5
     :geneotype (geneotype)
     :radius 6
     :type :guy}))

(defmulti influence-on-guy
  "How much a guy is influenced by something."
  :type)

(defmethod influence-on-guy :guy
  [other self]
  (if (or (identical? other self) (<= (other :life) 0))
      (num2 0 0)
      (let [delta (num2-sub (self :loc) (other :loc))]
        (num2-scale delta (/ ((self :geneotype) :avoidance) (Math/pow (num2-mag delta) 2))))))

(defmethod influence-on-guy :resource
  [other self]
  (if (<= (other :remaining) 0)
      (num2 0 0)
      (let [delta (num2-sub (other :loc) (self :loc))]
        (num2-scale delta (/ ((self :geneotype) :unhunger) (Math/pow (num2-mag delta) 2))))))

(defn guy-direction
  "Returns the direction a guy would like to move in, given a state."
  [self state]
    (num2-direction
      (reduce num2-sum (num2 0 0) 
              (map #(influence-on-guy % self) (state :things)))))

(defn guy-velocity
  "Returns the velocity a guy would move at, given a state."
  [self state]
    (num2-scale (guy-direction self state) guy-speed))

(defn resource
  ([]
    (resource 1.0 1.0))
  ([sim-width sim-height]
    {:loc (num2 (* (rand) sim-width) (* (rand) sim-height))
     :remaining 0.5
     :radius 3
     :type :resource}))

(defn colliding?
  [a b]
    (<= (num2-mag (num2-sub a b)) (+ (a :radius) (b :radius))))

(defn simulation
  "Yeah!"
  ([]
    (simulation 10 20 240 240))
  ([n-guys n-resources width height]
    (let [initial-state {
            :things (concat
              (for [_ (range n-guys)] (guy width height))
              (for [_ (range n-resources)] (resource width height)))}]
      {:initial-state initial-state
       :state (ref initial-state)
       :width width
       :height height})))

(defn guy-spawn
  "creates two offspring from a guy"
  [guy]
    (let
      [base-off (num2-scale (num2-at-angle-tau (rand)) (/ (guy :radius) 2))
       spawn (fn [mul-off]
        (assoc guy
          :life (/ (guy :life) 2)
          :geneotype (geneotype (guy :geneotype))
          :loc (num2-scale base-off mul-off)))]
      [ (spawn +1) (spawn -1) ]))

(defmulti advanced-thing
  "Returns a seq of what a thing is after a generation within a given state."
  (fn [guy state] (:type guy)))

(defmethod advanced-thing :guy
  [guy state]
    (let [new-guy (assoc guy :loc (num2-sum (guy :loc) (guy-velocity guy state))
                             :life (clamp01 (- (guy :life) guy-decay-rate)))]
         (cond
           (= (new-guy :life) 0)
            []
           (<= ((new-guy :geneotype) :life-to-repo) (new-guy :life))
            (guy-spawn new-guy)
           :else
            [new-guy])))

(defmethod advanced-thing :resource
  [res state]
    res)
    ; [(assoc res :remaining (clamp01 (+ (res :remaining) food-regrowth-rate)))])

(defn advanced-state
  "Returns what state becomes after a generation."
  [state]
    (assoc state :things (mapcat #(advanced-thing % state) (state :things))))

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
