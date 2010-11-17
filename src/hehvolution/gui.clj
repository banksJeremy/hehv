(ns hehvolution.gui
    (:require [hehvolution.core :as core])
    (:import (java.lang Thread)
             (javax.swing JFrame JPanel)
             (java.awt Color Graphics2D Dimension BasicStroke)
             (java.awt.geom RoundRectangle2D$Double Rectangle2D$Double)))

(defn rect
  "Creates a RectangularShape from [[x y] w] h [arcw [arch]]."
  ([s] (Rectangle2D$Double. 0 0 s s))
  ([w h] (Rectangle2D$Double. 0 0 w h))
  ([x y s] (Rectangle2D$Double. x y s s))
  ([x y w h] (Rectangle2D$Double. x y w h))
  ([x y w h arc] (RoundRectangle2D$Double. x y w h arc arc))
  ([x y w h arcw arch] (RoundRectangle2D$Double. x y w h arcw arch)))

(defn paint-guy
  "Paints a guy."
  [guy g2d scale]
    (.setPaint g2d Color/red)
    (.fill g2d (rect (* scale (guy :x)) (* scale (guy :y)) 8 8 3))
    (.setPaint g2d Color/black)
    (.draw g2d (rect (* scale (guy :x)) (* scale (guy :y)) 8 8 3)))

(defn paint-sim [sim scale])

; TODO: use agents and other idiomatic niceness
(defn run-simulation
  "Runs a simulation in a seperate thread."
  [sim hertz]
    (.start (Thread. (fn []
      (while true
        (Thread/sleep (* 1000 (/ 1 hertz)))
        (core/tick-sim sim))))))

(defn periodically-repaint
  "Yeah."
  [thing hertz]
    (.start (Thread. (fn []
      (while true
        (Thread/sleep (* 1000 (/ 1 hertz)))
        (.repaint thing))))))

(defn open-window
  "Yoah."
  ([] (open-window (core/simulation) 1))
  ([sim scale]
    (let [window (JFrame.)
          panel (proxy [JPanel] []
            (paintComponent [g] (let [g2d (cast Graphics2D g)]
              (proxy-super paintComponent g)
              (.setStroke g2d (BasicStroke. 2))
              (doseq [guy (@(sim :state) :guys)] (paint-guy guy g2d scale)))))]
       (.setPreferredSize panel (Dimension. (* (sim :width) scale)
                                            (* (sim :height) scale)))
       (.add window panel)
       (.pack window)
       (.setDefaultCloseOperation window JFrame/EXIT_ON_CLOSE)               
       (.show window)
       (run-simulation sim 10)
       (periodically-repaint panel 10)
       window)))
