(ns gayo.composer
  (:require [gayo.data :as gayo-data] 
            ["three" :as THREE] 
            ["three/examples/jsm/postprocessing/EffectComposer"
             :refer [EffectComposer]] 
            ["three/examples/jsm/postprocessing/RenderPass"
             :refer [RenderPass]] 
            ["three/examples/jsm/postprocessing/GlitchPass"
             :refer [GlitchPass]] 
            ["three/examples/jsm/postprocessing/OutlinePass"
             :refer [OutlinePass]] 
            [cljs-bean.core :refer [bean]]))

(defn composer
  [renderer scene camera]
  (let [c (EffectComposer. renderer)]
    (.addPass c (RenderPass. scene camera))
    c))



(comment
  (.addPass gayo-data/composer (RenderPass. gayo-data/scene (.-camera gayo-data/view)))
  
  (def gp (GlitchPass.))
  (.addPass gayo-data/composer gp)
  
  (set! (.. gayo-data/composer -passes -length) 0)
  
  (def a #js [])
  (def op (OutlinePass. (THREE/Vector2. 512 512) gayo-data/scene (.-camera gayo-data/view) a))  
  (.addPass gayo-data/composer op)
  
  (bean gayo-data/composer)
  
  (.pop (.-passes gayo-data/composer))
  
  (bean op)
  
  (set! (.-pulsePeriod op) 5)
  (set! (.-edgeThickness op) 500)
  (set! (.-edgeStrength op) 200) 
  (set! (.-edgeGlow op) 100)
  
  (set! (.-length a) 0)
  
  (doseq [v test.game/hitted]
    (.push a v))
  
  (.push a (first test.game/hitted))
  
  (set! (.-length a) 0)
  
  (set! (.-selectedObjects op) (into-array test.game/hitted))
  (set! (.-selectedObjects op) #js [])
  
  (bean gayo-data/composer)

  
  )
