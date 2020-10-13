(ns gayo.animation
  "Wrapper functions for dealing with three animation classes."
  (:require ["three" :as THREE]
            [gayo.data :refer [clips mixers]])
  (:require-macros [miracle.save :refer [save save-do]]))

(defn mixer*
  "Helper function for keeping track of mixers.
  One mixer per mesh."
  [mesh]
  (THREE/AnimationMixer. mesh))

(def mixer (memoize mixer*))

(defn clip*
  [animation-name]
  (.findByName THREE/AnimationClip clips animation-name))

(def clip (memoize clip*))

(defn play-animation!
  "Functional wrapper for
  1. creating or getting an AnimationMixer
  2. getting the clip
  3. playing the animation"
  ([mesh animation-name] (play-animation! mesh animation-name nil))
  ([mesh animation-name {:keys [loop cross-fade-from]}]
   (save :play-animation)
   
   (let [m (mixer mesh)
         c (clip animation-name)
         action (.clipAction m c)]
     (when-not (.-inited m)
       (.push mixers m)
       (set! (.-inited m) true))
     (if loop
       (.setLoop action THREE/LoopRepeat)
       (.setLoop action THREE/LoopOnce))
     (set! (.-clampWhenFinished action) true)
     
     (when cross-fade-from
       (.crossFadeFrom action cross-fade-from 0.2))
     
     (.reset (.play action)))))

(defn update-animations!
  "Needs to be run every frame."
  [dt]
  (doseq [m mixers]
    (.update m (* 0.001 dt))))



(comment
  (clip "Jaw Idle2")
  (clip "Jaw Idle2")
  )
