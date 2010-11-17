(ns hehvolution.gui
    (:require [hehvolution.core :as core])
    (:import (java.lang Thread)
             (javax.swing JFrame JPanel)
             (java.awt Color Graphics2D Dimension BasicStroke)
             (java.awt.geom RoundRectangle2D$Double)))

(defn rect
  ([s] (RoundRectangle2D$Double. 0 0 s s 0 0))
  ([w h] (RoundRectangle2D$Double. 0 0 w h 0 0))
  ([x y s] (RoundRectangle2D$Double. x y s s 0 0))
  ([x y w h] (RoundRectangle2D$Double. x y w h 0 0))
  ([x y w h arc] (RoundRectangle2D$Double. x y w h arc arc))
  ([x y w h arcw arch] (RoundRectangle2D$Double. x y w h arcw arch)))

(defn paint-guy [guy g2d scale]
  (.setPaint g2d Color/red)
  (.fill g2d (rect (* scale (guy :x)) (* scale (guy :y)) 8 8 3))
  (.setPaint g2d Color/black)
  (.draw g2d (rect (* scale (guy :x)) (* scale (guy :y)) 8 8 3)))

; TODO: use agents and other idiomatic niceness
(defn run-simulation [sim hertz]
  (.start (Thread. (fn []
    (while true
      (Thread/sleep (* 1000 (/ 1 hertz)))
      (dosync (alter (sim :state) (fn [old_state]
        (assoc old_state :guys
               (map (fn [old] (assoc old :x (+ 1 (old :x))))
                    (old_state :guys)))))))))))

(defn periodically-repaint [thing hertz]
  (.start (Thread. (fn []
    (while true
      (Thread/sleep (* 1000 (/ 1 hertz)))
      (.repaint thing))))))

(defn open-window
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
