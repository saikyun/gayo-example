(ns gayo.text
  (:require [gayo.data :as gayo-data]
            ["three" :as THREE]
            [gayo.assets :as assets]
            [gayo.tweens :as tw]
            [gayo.data :refer [clj-meshes]]
            [promesa.core :as p]
            
            [clojure.string :as str]
            [cljs.reader :as read]
            
            [gayo.scene :refer [go->top-go]]
            [gayo.hooks :as h]
            [gayo.state :refer [ensure-state!]]
            [gayo.bmfont :as bmfont :refer [default-opts default-opts-clj]]
            [gayo.log :refer [log!]])
  (:require-macros [miracle.save :refer [save save-do]]))

(defonce shader nil)
(defonce text-texture nil)

(def measurer (THREE/Box3.))

;; scale x => w y
;; ? => maxWidth / (x / y)

(defn set-pos!
  ([text-mesh]
   (set-pos! text-mesh default-opts))
  ([text-mesh opts]
   (.. text-mesh -scale (set 1 1 1))
   (let [scale-down (.-scaleDown opts)
         scale (.-scale opts)
         scale      
         (if-not scale-down
           scale
           (do
             (.setFromObject measurer text-mesh)
             (let [w (- (.. measurer -max -x)
                        (.. measurer -min -x))]
               (if (> w (/ (.-maxWidth opts) scale))
                 (save-do :ler-set
                          (- scale
                             (/ (* w 3) (.-maxWidth opts))))
                 scale))))
         pos (.-pos opts)]
     (save :ye-set-pos)
     
     (.. text-mesh -scale (set scale scale scale))
     (let [z (* scale 0.5 (.-yOffset text-mesh))]
       (.set (.-position text-mesh) 0 (+ 0.02 (if (> z 0)
                                                0
                                                (* (- z) 0.8))) z))
     (when pos
       (.add (.-position text-mesh) pos))
     text-mesh)))

(defn set-text!
  ([mesh t]
   (set-text! mesh t default-opts))
  ([mesh t opts]
   (ensure-state! mesh)           
   (set! (.. mesh -state -textContent) t)
   (if-not shader
     (log! "shader not loaded")
     (if-not text-texture
       (log! "text-texture not loaded")
       (if-not (.-add mesh)
         (do (log! "Mesh is missing `.add`" mesh)
             (def bad-mesh mesh))
         (let [tm (some-> mesh .-state .-text)]
           (if (and tm (= t (.-textContent tm)))
             (set-pos! tm opts)
             (let [text-mesh (bmfont/create-text!
                              shader
                              text-texture
                              t
                              gayo-data/max-ani
                              opts)]
               (ensure-state! text-mesh)
               (when-let [old-tm (.. mesh -state -text)]
                 (.remove mesh old-tm))
               (set! (.. mesh -state -text) text-mesh)
               (set-pos! text-mesh opts)
               (.add mesh text-mesh)
               text-mesh))))))))
