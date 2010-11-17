(ns hehvolution.gui
    (:require [hehvolution.core :as core])
    (:import (java.lang Thread Float)
             (javax.swing JFrame JPanel)
             (java.awt Color Graphics2D Dimension BasicStroke)
             (java.awt.image BufferedImage)
             (java.awt.geom RoundRectangle2D$Double Rectangle2D$Double)))

(defn rect
  "Creates a RectangularShape from [[x y] w] h [arcw [arch]]."
  ([s] (Rectangle2D$Double. 0 0 s s))
  ([w h] (Rectangle2D$Double. 0 0 w h))
  ([x y s] (Rectangle2D$Double. x y s s))
  ([x y w h] (Rectangle2D$Double. x y w h))
  ([x y w h arc] (RoundRectangle2D$Double. x y w h arc arc))
  ([x y w h arc-w arc-h] (RoundRectangle2D$Double. x y w h arc-w arc-h)))

(defn paint-scaled-rect
  "Paints a scaled [rounded] rectangle in a graphics context."
  [g2d scale fill-color border-color border-width & rect-args]
    (let [rect-args (map #(* scale %) rect-args)]
      (.setPaint g2d fill-color)
      (.fill g2d (apply rect rect-args))
      (.setPaint g2d border-color)
      (.setStroke g2d (BasicStroke. (* scale border-width)))
      (.draw g2d (apply rect rect-args))))

(defn guy-fill
  "Determines the fill color for a guy, based on his geneotype."
  [guy]
    (apply #(Color. %1 %2 %3) ; can't apply to "Color." =/
           (for [gene core/genes] (Float. ((guy :geneotype) gene)))))

(defn guy-border
  "Determines the border color for a guy, based on his life."
  [guy]
    (Color/getHSBColor 0 0 (guy :life)))

(defn paint-guy
  "Paints a guy."
  [g2d scale guy]
    (if (> (guy :life) 0)
        (paint-scaled-rect g2d scale (guy-fill guy) (guy-border guy) 1.5
                           ((guy :loc) :x) ((guy :loc) :y) 6 6 1)))

(defn paint-resource
  [g2d scale res]
    (if (> (res :remaining) 0)
        (paint-scaled-rect g2d scale Color/green Color/blue 0.5
                           ((res :loc) :x) ((res :loc) :y) 3 3 0.5)))

(defn paint-sim
  "Paints a sim."
  [g2d scale sim]
    (let [state @(sim :state)]
      (doseq [guy (state :guys)] (paint-guy g2d scale guy))
      (doseq [res (state :resources)] (paint-resource g2d scale res))))

; TODO: use agents and other idiomatic niceness
(defn run-simulation
  "Runs a simulation in a seperate thread."
  [sim hertz]
    (.start (Thread. (fn []
      (while true
        (Thread/sleep (* 1000 (/ 1 hertz)))
        (core/tick-sim sim))))))

(defn frequently-repaint
  "Yeah."
  [thing hertz]
    (.start (Thread. (fn []
      (while true
        (Thread/sleep (* 1000 (/ 1 hertz)))
        (.repaint thing))))))

(defn open-window
  "Yoah."
  ([] (open-window (core/simulation) 5))
  ([sim scale]
    (let [window (JFrame.)
          panel (proxy [JPanel] []
            (paintComponent [g]
              (proxy-super paintComponent g)
              (let [image (BufferedImage. (* (sim :width) scale)
                                          (* (sim :height) scale)
                                          BufferedImage/TYPE_INT_ARGB)
                    g2d (.createGraphics image)]
                   (paint-sim g2d scale sim)
                   (.drawImage g image 0 0 nil))))]
       (.setPreferredSize panel (Dimension. (* (sim :width) scale)
                                            (* (sim :height) scale)))
       (.add window panel)
       (.pack window)
       ; (.setDefaultCloseOperation window JFrame/EXIT_ON_CLOSE)               
       (.show window)
       (run-simulation sim 30)
       (frequently-repaint panel 30)
       window)))
