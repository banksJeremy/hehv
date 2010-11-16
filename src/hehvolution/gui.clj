(ns hehvolution.gui
    (:require [hehvolution.core :as core])
    (:import (javax.swing JFrame JPanel)
             (java.awt Color Graphics2D Dimension)
             (java.awt.geom RoundRectangle2D$Double)))

(defn rect
  ([x y s] (rect x y s s 0 0))
  ([x y w h] (rect x y w h 0 0))
  ([x y w h arc] (rect x y w h arc arc))
  ([x y w h arcw arch] (RoundRectangle2D$Double. x y w h arcw arch)))

(defn open-window []
  (let [window (JFrame.)
        panel (proxy [JPanel] []
          (paintComponent [g] (let [g2d (cast Graphics2D g)]
            (proxy-super paintComponent g)
            (.setColor g2d Color/red)
            (.fill g2d (rect 5 5 50 50 10))
            (.setColor g2d Color/black)
            (.draw g2d (rect 5 5 50 50 10)))))]
       (.setPreferredSize panel (Dimension. 300 300))
       (.add window panel)
       (.pack window)
       (.setDefaultCloseOperation window JFrame/EXIT_ON_CLOSE)               
       (.show window)
       window))
