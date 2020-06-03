(ns gayo.three-utils
  (:require [gayo.data :as gayo-data]
            #?@(:browser [["three" :as THREE]]
                :rn [["expo-three" :as ExpoTHREE]])))

#?(:rn
   (do (def TextureLoader ExpoTHREE/TextureLoader)
       
       (defn renderer [gl]
         (ExpoTHREE/Renderer. #js {:gl gl, :pixelRatio 1})))
   
   :browser
   (do (def TextureLoader THREE/TextureLoader)
       
       (defn renderer [_]
         (let [r (THREE/WebGLRenderer.)]
           (.setPixelRatio r (.. js/window -devicePixelRatio))
           (.. js/document -body (appendChild (.-domElement r)))
           r))))


(defn visible-height
  [depth camera]
  (let [cameraOffset (.. camera -position -z)
        ;; compensate for cameras not positioned at z=0
        depth (if (< depth cameraOffset)
                (- depth cameraOffset)
                (+ depth cameraOffset))
        ;; vertical fov in radians
        vFOV (* (.-fov camera) (/ js/Math.PI 180))
        ;; Math.abs to ensure the result is always positive
        ]
    (* 2
       (js/Math.tan (/ vFOV 2))
       (js/Math.abs depth))))

(defn visible-width
  [depth camera]
  (let [height (visible-height depth camera)]
    (* height (.-aspect camera))))



(defn ab->str
  [buffer]
  (let [bytes (js/Uint8Array. buffer)
        binary (js/String.)
        len (.-byteLength bytes)]
    (loop [i 0
           binary binary]
      (if (< i len)
        (recur (inc i) (.concat binary (.fromCharCode js/String (aget bytes i))))
        binary))))
