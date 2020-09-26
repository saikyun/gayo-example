(ns gayo.sprite
  (:require ["three" :as THREE]))

(defn sprite!
  [texture-or-opts]
  (let [sm (THREE/SpriteMaterial.
            (if (= THREE/Texture (type texture-or-opts))
              #js {:color 0xffffff :map texture-or-opts}
              texture-or-opts))
        s (THREE/Sprite. sm)]
    s))
