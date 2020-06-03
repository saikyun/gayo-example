(ns gayo.state)

(defn ensure-state!
  [o]
  (when-not (.-state o)
    (set! (.-state o) #js {})))
