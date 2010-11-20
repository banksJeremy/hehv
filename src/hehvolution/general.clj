(ns hehvolution.general
    "Code that need not be specific to this application.")

(defn thread-running [f]
  (let [thread (Thread. f)]
    (.start thread)
    thread))

(defn clamp
  [value low high]
    (cond
      (< value low) low
      (> value high) high
      :else value))

(defn sqrt
  [x]
    (Math/pow x 0.5))
