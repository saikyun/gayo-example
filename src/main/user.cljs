(ns user
  (:require [gayo.internal]))

(defn refresh!
  []
  (gayo.internal/render! 0.1))
