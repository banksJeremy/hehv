(ns hehvolution.core
    "Simulation!!!"
    (:use hehvolution.general))

(def mutation-stdev 0.1)

(def genes [:unhunger :avoidance :life-to-repo])

; later as many of these will be sim-specific as practical
(def guy-max-radius 9)
(def res-max-radius 4)
(def guy-speed 0.2)
(def guy-decay-rate 0.0002)
(def food-regrowth-rate 0.01)
(def food-consumption-rate 0.02)

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

(defmethod influence-on-guy :res
  [other self]
  (if (<= (other :life) 0)
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

(defn guy-radius-given-life
  [life]
    (* guy-max-radius (sqrt life)))

(defn res-radius-given-life
  [life]
    (* res-max-radius (sqrt life)))

(defmulti thing-radius :type)

(defmethod thing-radius :guy
  [guy] (guy-radius-given-life (guy :life)))

(defmethod thing-radius :res
  [res] (res-radius-given-life (res :life)))

(defn resource
  ([]
    (resource 1.0 1.0))
  ([sim-width sim-height]
    {:loc (num2 (* (rand) sim-width) (* (rand) sim-height))
     :life 0.5
     :type :res}))

(defn things-outer-distance
  ([a b]
    (- (num2-mag (num2-sub (a :loc) (b :loc))) (thing-radius a) (thing-radius b))))

(defn things-colliding
  ([a b]
    (<= (things-outer-distance a b) 0)))

(defn simulation
  "Yeah!"
  ([]
    (simulation 10 20 240 240))
  ([n-guys n-res width height]
    (let [initial-state {
            :things (concat
              (for [_ (range n-guys)] (guy width height))
              (for [_ (range n-res)] (resource width height)))}]
      {:initial-state initial-state
       :state (ref initial-state)
       :width width
       :height height})))

(defn guy-spawn
  "creates two offspring from a guy"
  [guy]
    (let
      [base-off (num2-scale (num2-at-angle-tau (rand)) (guy-radius-given-life (/ (guy :life) 2)))
       spawn (fn [mul-off]
        (assoc guy
          :life (/ (guy :life) 2)
          :geneotype (geneotype (guy :geneotype))
          :loc (num2-sum (guy :loc) (num2-scale base-off mul-off))))]
      [ (spawn +1) (spawn -1) ]))

(defn thing-alter-life
  [thing amount]
    (assoc thing :life (clamp01 (+ (thing :life) amount))))

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

(defmethod advanced-thing :res
  ([res state]
    (def consumers (filter
      #(and
        (things-colliding res %)
        (= (% :type) :guy))
      (state :things)))
    
    (if (seq consumers)
        (do
          
          ; grant life
          
          (def advanced (assoc res :life (- (res :life) life-loss)))
          (if (> (advanced :life) 0)
              [advanced]
              nil))
        [(thing-alter-life res food-regrowth-rate)])))

(defn state-consumption
  ; a map of things to their life changes due to consumption in this tick
  ([state]
    (def consumers-by-consumed
      (loop [consumers {} remaining (filter #(= (% :type) :res) (state :things))]
        (def current (first remaining))
        (def others (rest remaining))
        (if (seq others)
            (recur (assoc consumers current
                          (filter #(and (= (% :type :res))
                                        (things-colliding current %))
                                  (state :things)))
                   others)
            consumers)))
    
    (defn consumptions [[consumed consumers]]
      (if (seq consumers)
          (let
            [total-consumed (min (* food-consumption-rate (count consumers)) (consumed :life))
             each-consumed (/ total-consumed (count consumers))]
            (concat [{ :thing consumed :delta (- total-consumed) }]
                    (for [consumer consumers] {:thing consumer :delta each-consumed})))
          nil))
    
    (def consumption-by-consumer
      (loop [changes {} remaining consumption-by-consumer]
        (def current (first remaining))
        (def others (rest remaining))
        
        (loop [changes changes remaining (consumptions current)]
          ; this code is getting horrible. stop that.
        )
        
        
        )
      
      )
    
    ))

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
