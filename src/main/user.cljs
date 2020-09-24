(ns user
  (:require [gayo.data]
            [gayo.internal]))

(defn only-render
  []
  (.render gayo.data/composer))

(defn refresh!
  []
  (gayo.internal/render! 0.1))
