(ns hehvolution.graphical-general
    "Graphical code that need not be specific to this application."
    (:use hehvolution.general)
    (:import
      (java.awt Color BasicStroke)
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
  "Creates an ellipse or circle given [x y] r[x ry].
  The coordinates corresond to the centre of the shape, not the corner."
  ([r] (circ 0 0 r r))
  ([x y r] (circ x y r r))
  ([x y rx ry] (Ellipse2D$Float. (- x (/ rx 2)) (- y (/ ry 2))
                                 (* rx 2) (* ry 2))))

(defn paint-scaled-rect
  "Paints a scaled [rounded] rectangle in a graphics context."
  [g2d scale fill-color border-color border-width & rect-args]
    (let [rect-args (map #(* scale %) rect-args)]
      (.setPaint g2d fill-color)
      (.fill g2d (apply rect rect-args))
      (.setPaint g2d border-color)
      (.setStroke g2d (BasicStroke. (* scale border-width)))
      (.draw g2d (apply rect rect-args))))
