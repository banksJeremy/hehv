(ns hehvolution.graphical
    "Provides simulation visualizations."
    (:use hehvolution.graphical-general)
    (:require [hehvolution.core :as core])
    (:import
      (javax.swing JFrame JPanel)
      (java.awt Color Graphics2D Dimension)
      (java.awt.image BufferedImage)))

(defn guy-fill
  "Determines the fill color for a guy, based on his geneome."
  [guy]
    (apply #(Color. %1 %2 %3) ; can't apply to "Color." =/
           (for [gene core/genes] (Float. ((guy :geneome) gene)))))

(defn guy-border
  "Determines the border color for a guy, based on his life."
  [guy]
    (Color/getHSBColor 0 0 (guy :life)))

(defmulti paint-thing :type)

(defmethod paint-thing :guy
  [g2d scale guy]
        (paint-scaled-rect g2d scale (guy-fill guy) (guy-border guy) 1.5
                           ((guy :loc) :x) ((guy :loc) :y) (guy :radius) (guy :radius) 1))

(defmethod paint-thing :resource
  [g2d scale res]
    (if (> (res :remaining) 0)
        (paint-scaled-rect g2d scale Color/green Color/blue 0.5
                           ((res :loc) :x) ((res :loc) :y) (res :radius) (res :radius) 0.5)))

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

(defn visualization-display
  ([vis hertz]
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
       (frequently-repaint panel hertz)
       window)))

(defn sim-run-and-display
  ([sim] (sim-run-and-display 30 sim 2.0))
  ([sim hertz] (sim-run-and-display hertz sim 2.0))
  ([sim hertz scale]
    (visualization-display (visualization sim scale) hertz)
    (core/sim-frequently-tick sim hertz)))
