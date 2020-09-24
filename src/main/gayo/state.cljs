(ns gayo.state
  (:require [reagent.core :as r])
  (:require-macros [gayo.state]))

(defonce watched-o (r/atom nil))

(defn watch
  [o]
  
  (set! (.. o -state)
        (js->clj (.. o -state)))
  
  (reset! watched-o o))

#_(defn ensure-state!
    [o]
    (when-not (.-state o)
      (set! (.-state o) #js {})))

(defn ensure-state!
  [o]
  (when-not (.-state o)
    (set! (.-state o) #js {})))
