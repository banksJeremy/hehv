(ns hehvolution.graphical
    "Provides simulation visualizations."
    (:use hehvolution.general hehvolution.graphical-general)
    (:require [hehvolution.core :as core])
    (:import
      (javax.swing JFrame JPanel KeyStroke AbstractAction)
      (java.awt Color Graphics2D Dimension Toolkit)
      (java.awt.event ActionEvent KeyEvent WindowAdapter)
      (java.awt.image BufferedImage)))

(defmulti thing-fill :type)
(defmethod thing-fill :guy
  [guy]
    (apply #(Color. %1 %2 %3) ; can't apply to "Color." =/
           (for [gene core/genes] (Float. ((guy :geneotype) gene)))))

(defmethod thing-fill :res [res] (Color. 0 255 0 128))
(defmulti thing-border :type)
(defmethod thing-border :res [res] Color/blue)
(defmethod thing-border :guy [guy] Color/black)

(defn paint-thing
  [thing g2d vis]
        (paint-scaled centered-circ
          g2d (vis :scale) (thing-fill thing) (thing-border thing) 0
          ((thing :loc) :x) ((thing :loc) :y) (core/thing-radius thing)))

(defn visualization
  ([sim] (visualization sim 1.0))
  ([sim scale] {:sim sim :scale scale}))

(defn visualization-dimensions
  [vis] [(* (vis :scale) ((vis :sim) :width))
         (* (vis :scale) ((vis :sim) :height))])

(defn visualization-render
  "Renders the current state of the simulation in a BufferedImage."
  ([vis]
    (visualization-render
      vis (apply #(BufferedImage. %1 %2 BufferedImage/TYPE_INT_ARGB)
                 (visualization-dimensions vis))))
  ([vis image]
    (let [g (.createGraphics image)]
         (doseq [thing (@((vis :sim) :state) :things)]
                (paint-thing thing g vis)))
    image))

(defn visualization-display-close
  [dis]
    (dosync (alter (dis :alive) (fn [_] false)))
    (.setVisible (dis :win) false)
    (.dispose (dis :win)))

(defn repeatedly-repaint-display
  ([dis]
    (repeatedly-repaint-display dis 10))
  ([dis poll-delay]
    (thread-running (fn f
      ([]
        (f nil))
      ([previous-state]
        (if @(dis :alive)
            (let [new-state @(((dis :vis) :sim) :state)]
                 (if-not (identical? new-state previous-state)
                         (.repaint (dis :panel)))
                 (Thread/sleep poll-delay)
                 (recur new-state))))))))

(defn visualization-display
  ([vis nil-on-close] (visualization-display nil))
  ([vis]
    (let [window (JFrame.)
          panel (proxy [JPanel] []
            (paintComponent [g]
              (proxy-super paintComponent g)
              (.drawImage g (visualization-render vis) 0 0 nil)))]
      (.setPreferredSize
        panel (apply #(Dimension. %1 %2)(visualization-dimensions vis)))
      (.add window panel)
      (.pack window)
      (.setResizable window false)
      (.show window)
      (let
        [dis {:vis vis
              :win window
              :panel panel
              :alive (ref true)}
         cW (KeyStroke/getKeyStroke KeyEvent/VK_W (.getMenuShortcutKeyMask (Toolkit/getDefaultToolkit)))]
        (.put (.getActionMap panel) "close-window"
              (proxy [AbstractAction] ["Close Window"]
                (actionPerformed [e]
                  (visualization-display-close dis))))
        (.put (.getInputMap panel JPanel/WHEN_IN_FOCUSED_WINDOW) cW "close-window")
        (repeatedly-repaint-display dis)
        (.addWindowListener window (proxy [WindowAdapter] []
          (windowClosing [e]
            (visualization-display-close dis)))) ; this going to try to close twice?
        dis))))

(defn sim-run-and-display
  ([sim] (sim-run-and-display sim 30 3.0))
  ([sim hertz] (sim-run-and-display sim hertz 3.0))
  ([sim hertz scale]
    (let [dis (visualization-display (visualization sim scale))]
         (core/sim-frequently-tick sim hertz (dis :alive))
         dis)))

(defn go [] (sim-run-and-display (core/simulation)))
