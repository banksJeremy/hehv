(ns hehvolution.general
    "Code that need not be specific to this application.")

(defn thread-running [f]
  (let [thread (Thread. f)]
    (.start thread)
    thread))
