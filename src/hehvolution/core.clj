(ns hehvolution.core)

(defmacro def- [symbol value]
  `(def ^{:private true} ~symbol ~value)); TODO: not ignore existing meta
