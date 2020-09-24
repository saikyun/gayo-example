(ns boat.targets.browser
  (:require [promesa.core :as p]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            
            [miracle.soar :as ms]
            
            [gayo.data :as gayo-data]
            [gayo.path :refer [path]]
            [gayo.internal :as internal]
            [gayo.bmfont :as bmfont]            
            [gayo.text :as t]
            
            [user]
            
            [boat.game :as game]))

(defonce gameboard (js/require "../../../assets/dumb_map.gltf"))

(defn start-game
  [k]
  (p/then (internal/on-context-create nil (path gameboard))
          #(internal/deal-with % k game/init))
  
  (set! gayo-data/update! #'game/update!)
  (set! gayo-data/move #'game/move)
  (set! gayo-data/start #'game/start)
  (set! gayo-data/release #'game/release))

(defn preload!
  []
  (-> (bmfont/init-text)
      (p/then (fn [[s texture]]
                (set! t/shader s)
                (set! t/text-texture texture)))))


(defn ^:export init []
  (r/with-let [_ (preload!)]
    (set! gayo-data/dont-render true)
    (start-game :first-real-game))

  (ms/start))
