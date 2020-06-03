(ns gayo.particles
  (:require ["three" :as THREE]
            
            [gayo.hooks :as h]
            [gayo.data :as gayo-data]
            [gayo.assets :as assets]
            
            ["three-bas" :as BAS]
            
            [cljs-bean.core :refer [bean]]
            [miracle.save :refer-macros [save save-do]]))

(def rfs (.-randFloatSpread THREE/Math))

(defn material
  []
  (BAS/PointsAnimationMaterial.
   #js {;; material parameters/flags go here
        :depthWrite false
        :blending THREE/AdditiveBlending 
        :transparent true
        :vertexColors THREE/VertexColors
        :vertexColor #js [
                          "vColor = aColor;"
                          ],
        ;; custom uniform definitions
        :uniforms #js {
                       ;; uTime is updated every frame, and is used to calculate the current animation state
                       ;; this is the only value that changes, which is the reason we can animate so many objects at the same time
                       :uTime #js {:value 0}
                       }
        ;; uniform *values* of the material we are extending go here
        :uniformValues #js {
                            :size 30.0
                            :sizeAttenuation true
                            },
        ;; BAS has a number of functions that can be reused. They can be injected here.
        :vertexFunctions #js [(aget BAS/ShaderChunk "ease_cubic_in_out")
                              (aget BAS/ShaderChunk "ease_quad_out")
                              (aget BAS/ShaderChunk "ease_quad_in")
                              (aget BAS/ShaderChunk "ease_quart_out")
                              (aget BAS/ShaderChunk "ease_circ_out")
                              (aget BAS/ShaderChunk "ease_back_in")]
        ;; parameter  must match uniforms and attributes defined above in both name and type
        ;; as a convention, I prefix uniforms with 'u' and attributes with 'a' (and constants with 'c', varyings with 'v', and temps with 't')
        :vertexParameters
        #js ["uniform float uTime;"
             "attribute vec3 aColor;"
             "attribute vec2 aDelayDuration;"
             "attribute vec3 aStartPosition;"
             "attribute vec3 aEndPosition;"]
        ;; this chunk is injected 1st thing in the vertex shader main() function
        ;; variables declared here are available in all subsequent chunks
        :vertexInit
        #js ["float tProgress = clamp(uTime - aDelayDuration.x, 0.0, aDelayDuration.y) / aDelayDuration.y;",
             "float tProgress2 = easeCircOut(tProgress);"]
        
        :fragmentShape
        #js ["float distanceToCenter = distance(gl_PointCoord, vec2(0.5));
              float pct = 1.0 - smoothstep(0.0, 0.5, distanceToCenter);
              gl_FragColor = vec4(gl_FragColor.rgb, pct * gl_FragColor.a);"]
        
        :vertexPosition
        #js [
             "vec3 gravity = vec3(0, 0, 1000);"
             "transformed += mix(aStartPosition, mix(aEndPosition, gravity, easeQuadIn(tProgress)), tProgress2);"
             ]
        }))

(defn getRandomPointOnSphere
  [r target]
  (let [u (rand)
        v (rand)
        theta (* 2 (.-PI js/Math) u)
        phi (.acos js/Math (dec (* 2 v)))
        x (* r (.sin js/Math theta) (.sin js/Math phi))
        y 0 #_ (* r (.cos js/Math theta) (.sin js/Math phi))
        z (* r (.cos js/Math phi))]
    (.set target x y z)))

(defn geom
  []
  ;; the geometry that will be used by the PrefabBufferGeometry
  ;; any Geometry will do\, but more complex ones can be repeated less often
  (let [ ;; the number of times the prefabGeometry will be repeated
        prefabCount 300
        
        ;; BAS.PrefabBufferGeometry extends THREE/BufferGeometry
        ;; it stores data that is used for animation calculation in addition to the actual geometry
        geometry (BAS/PointBufferGeometry. prefabCount)
        
        ;; create a BufferAttribute with an item size of 2
        ;; each prefab has an animation duration and a delay
        ;; these will be used to calculate the animation state within the vertex shader
        duration 0.8
        
        aDelayDuration (.createAttribute geometry "aDelayDuration" 2
                                         (fn [data index num]
                                           (doseq [i (range num)]
                                             (aset data 0 (rand-int 0.3))
                                             (aset data 1 duration))))
        maxPrefabDelay 0.5]
    
    ;; create two BufferAttributes with an item size of 3
    ;; these will store the start and end position for the translation
    (let [r 400
          aStartPosition 
          (.createAttribute
           geometry
           "aStartPosition" 3
           (fn [data index count]
             (let [sv (THREE/Vector3.)]
               (set! (.-x sv) 0)
               (set! (.-y sv) 0)                    
               (set! (.-z sv) 0)
               (.toArray sv data))))
          
          radius 10
          
          aEndPosition 
          (.createAttribute
           geometry
           "aEndPosition" 3
           (fn [data index count]
             (let [sv (THREE/Vector3.)]
               (getRandomPointOnSphere radius sv)
               (.toArray sv data))))
          
          c (THREE/Color.)
          options [0.2 0.4 0.6 0.8 1.0 1.2 1.4]
          hue (* 0.1 (rand-int 10))
          aColor (.createAttribute geometry "aColor" 3
                                   (fn [data index count]
                                     (.setHSL c
                                              hue
                                              (+ 0.4 (rand 0.2)) 
                                              (+ 0.4 (rand 0.2)))
                                     (.toArray c data)))]
      geometry)))

(defn YeBoi
  []
  (this-as this
    (.call THREE/Points this (geom) (material))
    (set! (.-frustumCulled this) false)))


(set! (.-prototype YeBoi) (.create js/Object (.-prototype THREE/Points)))  
(set! (.. YeBoi -prototype -constructor) YeBoi)      


(.defineProperty 
 js/Object
 (.-prototype YeBoi)
 "time"
 #js {:get #(this-as this (-> this .-material .-uniforms (aget "uTime") .-value))
      :set #(this-as this (set! (.-value (-> this .-material .-uniforms (aget "uTime")))
                                %))})    

(def lul #js {})

(def has-won false)

(defn reset-anim
  [h]
  (js/setTimeout
   (fn []
     (when has-won
       (set! (.-time h) 0)
       (reset-anim h)))
   (+ 2000 (rand-int 2000))))

(defn anim-pew 
  []
  (let [h (YeBoi.)
        k #js {}]
    (.. h -position (set (- (rand 3) 1.5) 0 #_ (- (rand 3) 1.5) (- (rand 3) 1.5)))
    (js/setTimeout
     (fn []
       (when has-won
         (.add gayo-data/scene h)
         (h/hook+ k :update :yee #(set! (.-time h) (+ (.-time h) 0.001)))
         (reset-anim h)))
     (rand-int 2000))))

(defn anim-3
  []
  (doseq [_ (range 5)]
    (anim-pew)))

(defn you-win!
  []
  (when-not has-won
    (.then (assets/load-texture "Victory Screen")
           #(let [sm (THREE/SpriteMaterial. #js {:map %})
                  sprite (THREE/Sprite. sm)]
              (def s sprite)
              
              (.add gayo-data/scene s)    
              
              (.set (.-position s) 0 10 3)    
              (.set (.-scale s) 2 2)
              
              (anim-3)))
    (set! has-won true)))

(defn you-lose!
  []
  (when-not has-won
    (.then (assets/load-texture "Lose Screen")
           #(let [sm (THREE/SpriteMaterial. #js {:map %})
                  sprite (THREE/Sprite. sm)]
              (.add gayo-data/scene sprite)
              
              (.set (.-position sprite) 0 10 3)    
              (.set (.-scale sprite) 2 2)))
    (set! has-won true)))

(comment
  
  
  
  (.remove gayo-data/scene s)
  
  
  )
