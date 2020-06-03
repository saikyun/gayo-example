(ns boat.game
  (:require [clojure.core.reducers :as r] 
            [clojure.string :as str]             
            ["three" :as THREE]  
            [promesa.core :as p]             
            
            [gayo.log :refer [log!]]            
            [gayo.data :as gayo-data]
            [gayo.state :refer [ensure-state!]] 
            [gayo.hooks :as hooks]
            [gayo.scene :as scene
             :refer [find-mesh-by-name
                     find-meshes-by-data
                     go->card
                     go->top-go
                     go->zone-go]] 
            [gayo.text :refer [set-text!]]
            [gayo.bmfont :refer [default-opts-clj]] 
            [gayo.tweens :as tw]             
            
            [miracle.save] 
            
            [cljs-bean.core :refer [bean]])
  (:require-macros [miracle.save :refer [save save-do]]))

(defonce raycaster (THREE/Raycaster.))
(def is-down false)
(def is-moving false)
(def is-moving-far false)
(def is-moving-super-far false)
(def down-pos (THREE/Vector2.))
(def curr-pos (THREE/Vector2.))
(def down-time nil)
(defonce point3d (THREE/Vector3.))
(defonce pos (THREE/Vector3.))

(defn two->three
  [point camera dist]
  (.set point3d (.-x point) (.-y point) 0.5)
  (.normalize (.sub (.unproject point3d camera) (.-position camera)))    
  (let [targetZ (- (.. camera -position -y) dist)
        distance (- (- (/ (- targetZ (.. camera -position -y)) (.-y point3d))))]
    (.add (.copy pos (.-position camera)) (.multiplyScalar point3d distance))
    pos))

(defn find-named
  [name]
  (find-mesh-by-name (.-scene gayo-data/loaded-scene) name))

(defn init
  [scene loaded-gltf conf-k]

  (let [light (THREE/AmbientLight. 0xffffff)]    
    (save :add-light1)
    (set! (.. light -intensity) 0.5)
    (.add scene light))
  
  )

(def hitted #js [])

(defn update!
  [_ camera _]
  (when (and is-down (not is-moving))
    (.setFromCamera raycaster down-pos camera)
    (let [intersects (.intersectObjects raycaster (.-children gayo-data/scene) true)]
      (set! hitted intersects))))

(defn start
  [point scene camera]
  
  (set! is-moving false)
  (set! is-moving-far false)
  (set! is-moving-super-far false)
  (set! is-down true)
  (.copy down-pos point)  
  (.copy curr-pos point)  
  
  (set! down-time gayo-data/last-time)
  
  (update! scene camera 0))

(defn move
  [point scene camera]
  (.copy curr-pos point) 
  
  (when (< 0.0005 (.distanceToSquared down-pos point))
    (set! is-moving true))
  
  (when (< 0.01 (.distanceToSquared down-pos point))
    (set! is-moving-far true))
  
  (when (< 0.5 (.distanceToSquared down-pos point))
    (set! is-moving-super-far true))    
  
  (.setFromCamera raycaster point camera)  
  (let [intersects (.intersectObjects raycaster (.-children scene) true)]
    
    ))

(defn release
  [point scene camera]
  (log! "release")
  
  (hooks/run-hooks! :release)
  (hooks/run-hooks! :click)
  
  (.setFromCamera raycaster point camera)
  (let [intersects (.intersectObjects raycaster (.-children scene) true)]
    
    )
  
  (set! is-moving false)
  (set! is-down false)) 
