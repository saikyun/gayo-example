(ns gayo.sprite
  (:require ["three" :as THREE]))

(defn sprite!
  [texture]
  (let [sm (THREE/SpriteMaterial. #js {:color 0xffffff :map texture}) 
        s (THREE/Sprite. sm)]
    s))


