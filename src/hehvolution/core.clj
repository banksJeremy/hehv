(ns hehvolution.core)

(defmacro def- [symbol value]
  `(def ^{:private true} ~symbol ~value)); TODO: not ignore existing meta

(defn simulation []
  {:state (ref {:guys [
      {:x 10 :y 20}
      {:x 30 :y 90}]})
   :width 120
   :height 120})
