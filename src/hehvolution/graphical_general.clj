(ns hehvolution.graphical-general
    "Graphical code that need not be specific to this application."
    (:use hehvolution.general)
    (:import
      (java.awt Color BasicStroke)
      (java.io File)
      (javax.imageio ImageIO)
      (java.awt.geom
        RoundRectangle2D$Float
        Rectangle2D$Float
        Ellipse2D$Float)))

(defn rect
  ([s] (Rectangle2D$Float. 0 0 s s))
  ([w h] (Rectangle2D$Float. 0 0 w h))
  ([x y s] (Rectangle2D$Float. x y s s))
  ([x y w h] (Rectangle2D$Float. x y w h))
  ([x y w h arc] (RoundRectangle2D$Float. x y w h arc arc))
  ([x y w h arc-w arc-h] (RoundRectangle2D$Float. x y w h arc-w arc-h)))

(defn circ
  "Creates an ellipse or circle given [x y] r[x ry]."
  ([r] (circ 0 0 r r))
  ([x y r] (circ x y r r))
  ([x y rx ry] (Ellipse2D$Float. (- x (/ rx 2)) (- y (/ ry 2))
                                 (* rx 2) (* ry 2))))

(defn centered-circ
  "Creates an elpise or circle given a center point instead of a corner one."
  ([r] (centered-circ 0 0 r r))
  ([x y r] (centered-circ x y r r))
  ([x y rx ry] 
    (let [xp (- x rx) yp (- y ry)]
         (circ xp yp rx ry))))

(defn paint-scaled
  "Um."
  [shape g2d scale fill border b-width & shape-args]
    (let [shape-args (map #(* scale %) shape-args)]
      (.setPaint g2d fill)
      (.fill g2d (apply shape shape-args))
      (.setPaint g2d border)
      (.setStroke g2d (BasicStroke. (* scale b-width)))
      (.draw g2d (apply shape shape-args))))

(defn dump-png
  [img filename]
    (ImageIO/write img "png" (File. filename)))
